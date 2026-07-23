// patch-output.mjs — fixes known rough edges when transpiling the full
// steampunk-2302-api definition library for local execution.
// Run after: npx abap_transpile
import { readdirSync, readFileSync, writeFileSync } from "node:fs";

const dir = "output";
let patched = 0;

for (const f of readdirSync(dir).filter(f => f.endsWith(".mjs"))) {
  const p = `${dir}/${f}`;
  let s = readFileSync(p, "utf-8");
  const orig = s;

  // 1) Octal literals like .set(06)
  s = s.replace(/\.set\(0([0-9])\)/g, ".set($1)");

  // 2) Static attribute "name"/"length" colliding with read-only JS class props
  s = s.replace(/^(\w+)\.(name|length) = (new abap\.types\.[^;]+);$/gm,
    'Object.defineProperty($1, "$2", {value: $3, writable: true});');

  // 3) Unsupported decfloat16/34 todo types
  s = s.replace(/new abap\.types\.typeTodoDecFloat(16|34)Type\(\)/g,
    "new abap.types.Float()");

  // 4) Raw NUL / replacement chars in Character.set()
  s = s.replace(/minchar\.set\(\x00\x00\)/g, "minchar.set('\\u0000')");
  s = s.replace(/maxchar\.set\(\uFFFD\uFFFD\)/g, "maxchar.set('\\uffff')");

  // 5) charsize placeholder '?'
  s = s.replace(/charsize\.set\('\?'\)/g, "charsize.set(1)");

  // 6) Unresolved bare-identifier constant refs in steampunk stubs,
  //    e.g. `.set(c_severity_status);` — only in non-Z (stub) files.
  if (!f.startsWith("z")) {
    s = s.replace(/\.set\(([a-z_][a-z0-9_$]*)\);$/gm, (m, id) =>
      (id === "abap" || s.includes(`let ${id}`) || s.includes(`const ${id}`))
        ? m : ".set('');");
  }

  if (s !== orig) { writeFileSync(p, s); patched++; }
}
console.log(`patched ${patched} files`);
