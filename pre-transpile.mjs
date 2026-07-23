#!/usr/bin/env node
// pre-transpile.mjs
// ---------------------------------------------------------------------------
// The abap-transpiler does not (yet) support a few modern ABAP constructs
// used in this repository. Instead of changing the original sources, this
// script copies /src to /src_transpile and rewrites the affected spots into
// transpiler-friendly, behavior-identical equivalents. The transpiler then
// reads from /src_transpile (see abap_transpile.json), while the repository
// and the SAP system keep the original, idiomatic syntax.
//
// Usage:  node pre-transpile.mjs      (run before: npx abap_transpile)
// ---------------------------------------------------------------------------
import { cpSync, rmSync, readFileSync, writeFileSync, existsSync } from "node:fs";

const SRC = "src";
const OUT = "src_transpile";
const MAIN = `${OUT}/zcl_cloud_logger.clas.abap`;

// -- 1. fresh copy of the sources -------------------------------------------
if (existsSync(OUT)) rmSync(OUT, { recursive: true });
cpSync(SRC, OUT, { recursive: true });
console.log(`Copied ${SRC}/ -> ${OUT}/`);

let s = readFileSync(MAIN, "utf-8");
let applied = 0;
const rewrite = (name, from, to) => {
  if (typeof from === "string" ? s.includes(from) : from.test(s)) {
    s = typeof from === "string" ? s.replace(from, to) : s.replace(from, to);
    applied++;
    console.log(`  [ok] ${name}`);
  } else {
    console.log(`  [--] ${name} - pattern not found (already compatible?)`);
  }
};

// -- 2. LET inside COND is not supported ("Let not supported, todo") --------
rewrite(
  "1/4  get_messages_flat: LET-in-COND -> LOOP",
  /RETURN VALUE #\( FOR msg IN log_messages\s*\(\s*COND flat_message\(\s*LET base[\s\S]*?ELSE base \) \) \)\./,
  `LOOP AT log_messages ASSIGNING FIELD-SYMBOL(<msg>).

      DATA(base) = COND string(
          WHEN <msg>-symsg-msgid IS INITIAL OR <msg>-symsg-msgno IS INITIAL
          THEN <msg>-message
          ELSE get_string_from_message( <msg>-symsg ) ).

      INSERT COND flat_message( WHEN <msg>-context IS NOT INITIAL
                                THEN |[{ <msg>-context }] { base }|
                                ELSE base ) INTO TABLE result.

    ENDLOOP.`
);

// -- 3. COND # type inference fails here ("TypeNameOrInfer: #") -------------
rewrite(
  "2/4  log_bapiret2_table_add: COND # -> explicit type",
  /DATA\(severity_filter\) = COND #\( WHEN min_severity IS NOT INITIAL THEN get_severity_filter\( min_severity \)\s*ELSE VALUE #\( \) \)\./,
  `DATA(severity_filter) = COND severity_filter_range( WHEN min_severity IS NOT INITIAL
                                                        THEN get_severity_filter( min_severity )
                                                        ELSE VALUE #( ) ).`
);

// -- 4. SWITCH # / inner VALUE # in get_severity_filter ---------------------
{
  const m = s.indexOf("METHOD get_severity_filter");
  const e = s.indexOf("ENDMETHOD", m);
  if (m > -1 && s.slice(m, e).includes("SWITCH #(")) {
    let body = s.slice(m, e)
      .replace("RETURN SWITCH #( msgty", "RETURN SWITCH severity_filter_range( msgty")
      .replaceAll("THEN VALUE #(", "THEN VALUE severity_filter_range(")
      .replace("ELSE VALUE #( )", "ELSE VALUE severity_filter_range( )");
    s = s.slice(0, m) + body + s.slice(e);
    applied++;
    console.log("  [ok] 3/4  get_severity_filter: SWITCH/VALUE # -> explicit types");
  } else {
    console.log("  [--] 3/4  get_severity_filter - pattern not found (already compatible?)");
  }
}

// -- 5. DELETE itab FROM 1 TO n not implemented ("DeleteInternalTodo") ------
rewrite(
  "4/4  trim_internal_errors: DELETE FROM..TO -> WHILE / DELETE INDEX",
  /IF lines\( internal_errors \) > me->trim_limit\.\s*DELETE internal_errors FROM 1 TO \( lines\( internal_errors \) - me->trim_limit \)\.\s*ENDIF\./,
  `WHILE lines( internal_errors ) > me->trim_limit.
      DELETE internal_errors INDEX 1.
    ENDWHILE.`
);

writeFileSync(MAIN, s);
console.log(`\n${applied} rewrite(s) applied -> ${OUT}/ is ready for npx abap_transpile`);
