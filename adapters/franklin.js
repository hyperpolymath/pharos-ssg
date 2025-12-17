// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Franklin.jl adapter - Static site generator in Julia
 * https://franklinjl.org/
 */

import { sanitizeJulia, isValidPort, sanitizePath } from "./_security.js";

export const name = "Franklin.jl";
export const language = "Julia";
export const description = "Static site generator for technical blogging in Julia with LaTeX support";

let connected = false;
let juliaPath = "julia";

async function runJulia(code, cwd = null) {
  const cmd = new Deno.Command(juliaPath, {
    args: ["-e", code],
    cwd: cwd || Deno.cwd(),
    stdout: "piped",
    stderr: "piped",
  });
  const output = await cmd.output();
  const decoder = new TextDecoder();
  return {
    success: output.success,
    stdout: decoder.decode(output.stdout),
    stderr: decoder.decode(output.stderr),
    code: output.code,
  };
}

export async function connect() {
  try {
    const cmd = new Deno.Command(juliaPath, {
      args: ["--version"],
      stdout: "piped",
      stderr: "piped",
    });
    const output = await cmd.output();
    connected = output.success;
    return connected;
  } catch {
    connected = false;
    return false;
  }
}

export async function disconnect() {
  connected = false;
}

export function isConnected() {
  return connected;
}

export const tools = [
  {
    name: "franklin_newsite",
    description: "Create a new Franklin site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new site" },
        template: { type: "string", description: "Template name" },
      },
    },
    execute: async ({ path, template }) => {
      const safePath = sanitizePath(path) || ".";
      const tmpl = template ? `; template="${sanitizeJulia(template)}"` : "";
      return await runJulia(`using Franklin; newsite("${sanitizeJulia(safePath)}"${tmpl})`);
    },
  },
  {
    name: "franklin_serve",
    description: "Start Franklin development server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        port: { type: "number", description: "Port number" },
        host: { type: "string", description: "Host to bind to" },
      },
    },
    execute: async ({ path, port, host }) => {
      const safePath = sanitizePath(path);
      let args = [];
      if (port && isValidPort(port)) args.push(`port=${port}`);
      if (host) args.push(`host="${sanitizeJulia(host)}"`);
      const argsStr = args.length ? args.join(", ") : "";
      return await runJulia(`using Franklin; serve(${argsStr})`, safePath);
    },
  },
  {
    name: "franklin_optimize",
    description: "Build optimized site for deployment",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        minify: { type: "boolean", description: "Minify output" },
        prerender: { type: "boolean", description: "Prerender pages" },
      },
    },
    execute: async ({ path, minify, prerender }) => {
      const safePath = sanitizePath(path);
      let args = [];
      if (minify !== false) args.push("minify=true");
      if (prerender !== false) args.push("prerender=true");
      const argsStr = args.length ? args.join(", ") : "";
      return await runJulia(`using Franklin; optimize(${argsStr})`, safePath);
    },
  },
  {
    name: "franklin_publish",
    description: "Publish site to GitHub Pages",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => {
      const safePath = sanitizePath(path);
      return await runJulia(`using Franklin; publish()`, safePath);
    },
  },
  {
    name: "franklin_version",
    description: "Get Julia/Franklin version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => {
      const cmd = new Deno.Command(juliaPath, {
        args: ["--version"],
        stdout: "piped",
        stderr: "piped",
      });
      const output = await cmd.output();
      const decoder = new TextDecoder();
      return {
        success: output.success,
        stdout: decoder.decode(output.stdout),
        stderr: decoder.decode(output.stderr),
      };
    },
  },
];
