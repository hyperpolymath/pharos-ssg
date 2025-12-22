// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// NoteG SSG - E2E Build Tests

import { assertEquals, assertExists } from "https://deno.land/std@0.208.0/assert/mod.ts";

Deno.test("Build system - Justfile exists", async () => {
  const stat = await Deno.stat("justfile");
  assertExists(stat);
});

Deno.test("Build system - Mustfile exists", async () => {
  const stat = await Deno.stat("Mustfile");
  assertExists(stat);
});

Deno.test("Build system - Containerfile exists", async () => {
  const stat = await Deno.stat("Containerfile");
  assertExists(stat);
});

Deno.test("Engine - Ada spec exists", async () => {
  const stat = await Deno.stat("engine/src/noteg_engine.ads");
  assertExists(stat);
});

Deno.test("Engine - Ada body exists", async () => {
  const stat = await Deno.stat("engine/src/noteg_engine.adb");
  assertExists(stat);
});

Deno.test("SSG - Types exist", async () => {
  const stat = await Deno.stat("ssg/src/Types.res");
  assertExists(stat);
});

Deno.test("SSG - Build module exists", async () => {
  const stat = await Deno.stat("ssg/src/Build.res");
  assertExists(stat);
});

Deno.test("Language - Lexer exists", async () => {
  const stat = await Deno.stat("noteg-lang/src/Lexer.res");
  assertExists(stat);
});

Deno.test("Language - Parser exists", async () => {
  const stat = await Deno.stat("noteg-lang/src/Parser.res");
  assertExists(stat);
});

Deno.test("Language - Compiler exists", async () => {
  const stat = await Deno.stat("noteg-lang/src/Compiler.res");
  assertExists(stat);
});

Deno.test("Language - Interpreter exists", async () => {
  const stat = await Deno.stat("noteg-lang/src/Interpreter.res");
  assertExists(stat);
});

Deno.test("Language - LSP server exists", async () => {
  const stat = await Deno.stat("noteg-lang/src/lsp/Server.res");
  assertExists(stat);
});

Deno.test("Accessibility - Schema exists", async () => {
  const stat = await Deno.stat("a11y/schema.json");
  assertExists(stat);
});

Deno.test("Accessibility - Schema is valid JSON", async () => {
  const content = await Deno.readTextFile("a11y/schema.json");
  const schema = JSON.parse(content);
  assertExists(schema.$schema);
  assertExists(schema.definitions.bslData);
  assertExists(schema.definitions.gslData);
  assertExists(schema.definitions.aslData);
  assertExists(schema.definitions.makatonData);
});

Deno.test("Adapters - Security module exists", async () => {
  const stat = await Deno.stat("adapters/_security.js");
  assertExists(stat);
});

Deno.test("Adapters - Zola adapter exists", async () => {
  const stat = await Deno.stat("adapters/zola.js");
  assertExists(stat);
});

Deno.test("SCM - ECOSYSTEM.scm exists", async () => {
  const stat = await Deno.stat("ECOSYSTEM.scm");
  assertExists(stat);
});

Deno.test("SCM - META.scm exists", async () => {
  const stat = await Deno.stat("META.scm");
  assertExists(stat);
});

Deno.test("SCM - STATE.scm exists", async () => {
  const stat = await Deno.stat("STATE.scm");
  assertExists(stat);
});
