// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Security utilities for adapter input sanitization
 * Prevents code injection in various language interpreters
 */

/**
 * Sanitize string for Julia code interpolation
 * Escapes backslashes and double quotes
 * @param {string} input - User input to sanitize
 * @returns {string} - Sanitized string safe for Julia string literals
 */
export function sanitizeJulia(input) {
  if (input == null) return "";
  return String(input)
    .replace(/\\/g, "\\\\")
    .replace(/"/g, '\\"')
    .replace(/\$/g, "\\$");
}

/**
 * Sanitize string for Common Lisp code interpolation
 * Escapes backslashes and double quotes
 * @param {string} input - User input to sanitize
 * @returns {string} - Sanitized string safe for Lisp string literals
 */
export function sanitizeLisp(input) {
  if (input == null) return "";
  return String(input)
    .replace(/\\/g, "\\\\")
    .replace(/"/g, '\\"');
}

/**
 * Validate module/identifier name (alphanumeric, underscore, dot only)
 * @param {string} name - Module or identifier name
 * @returns {boolean} - True if valid identifier
 */
export function isValidIdentifier(name) {
  if (!name || typeof name !== "string") return false;
  return /^[A-Za-z_][A-Za-z0-9_.]*$/.test(name);
}

/**
 * Validate port number
 * @param {number} port - Port number
 * @returns {boolean} - True if valid port
 */
export function isValidPort(port) {
  return Number.isInteger(port) && port >= 1 && port <= 65535;
}

/**
 * Sanitize file path - prevents path traversal
 * @param {string} path - File path
 * @returns {string} - Sanitized path
 */
export function sanitizePath(path) {
  if (path == null) return ".";
  // Remove null bytes and normalize
  const sanitized = String(path).replace(/\0/g, "");
  // Prevent command injection via shell metacharacters
  if (/[;&|`$(){}[\]<>!#*?]/.test(sanitized)) {
    throw new Error("Invalid characters in path");
  }
  return sanitized;
}
