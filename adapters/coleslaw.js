// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Coleslaw adapter - Static blog generator in Common Lisp
 * https://github.com/kingcons/coleslaw
 */

import { sanitizeLisp, isValidPort, sanitizePath } from "./_security.js";

export const name = "Coleslaw";
export const language = "Common Lisp";
export const description = "Flexible static blog/site generator written in Common Lisp";

let connected = false;
let sblPath = "sbcl";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(sblPath, {
    args: ["--script", ...args],
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

async function runColeslaw(lispExpr, cwd = null) {
  const cmd = new Deno.Command(sblPath, {
    args: ["--eval", `(ql:quickload :coleslaw)`, "--eval", lispExpr, "--quit"],
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
    const cmd = new Deno.Command(sblPath, {
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
    name: "coleslaw_init",
    description: "Initialize a new Coleslaw blog",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new blog" },
      },
    },
    execute: async ({ path }) => {
      const safePath = sanitizePath(path) || ".";
      return await runColeslaw(`(coleslaw:setup "${sanitizeLisp(safePath)}")`, safePath);
    },
  },
  {
    name: "coleslaw_build",
    description: "Build the Coleslaw site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => {
      const safePath = sanitizePath(path) || ".";
      return await runColeslaw(`(coleslaw:main "${sanitizeLisp(safePath)}")`, safePath);
    },
  },
  {
    name: "coleslaw_preview",
    description: "Preview the site locally",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        port: { type: "number", description: "Port number" },
      },
    },
    execute: async ({ path, port }) => {
      const safePath = sanitizePath(path);
      const p = port && isValidPort(port) ? port : 8080;
      return await runColeslaw(`(coleslaw:preview :port ${p})`, safePath);
    },
  },
  {
    name: "coleslaw_new_post",
    description: "Create a new post",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        title: { type: "string", description: "Post title" },
      },
      required: ["title"],
    },
    execute: async ({ path, title }) => {
      const safePath = sanitizePath(path);
      return await runColeslaw(`(coleslaw:new-post "${sanitizeLisp(title)}")`, safePath);
    },
  },
  {
    name: "coleslaw_version",
    description: "Get SBCL version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => {
      const cmd = new Deno.Command(sblPath, {
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
