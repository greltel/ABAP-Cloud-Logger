// run-tests.mjs — off-stack ABAP Unit runner for abap-cloud-logger.
// Loads the transpiled code, injects minimal JS mocks for the SAP platform
// classes (CL_BALI_*, XCO_CP*, CL_ABAP_CONTEXT_INFO) that only exist as
// signature stubs in steampunk-2302-api, then runs the full test suite and
// reports pass/fail per method instead of stopping at the first error.
import "./output/init.mjs";

const A = abap; // transpiler runtime global

const str = (v) => new A.types.String().set(v);
const chr = (len, v) => new A.types.Character(len).set(v);
const obj = (qname, pointer) => {
  const o = new A.types.ABAPObject({ qualifiedName: qname, RTTIName: "\\INTERFACE=" + qname });
  o.set(pointer);
  return o;
};
const plain = (v) => (v && v.get ? String(v.get()) : String(v ?? ""));

// ---------- CL_ABAP_CONTEXT_INFO ----------
A.Classes["CL_ABAP_CONTEXT_INFO"].get_user_alias = async () => chr(12, "OFFSTACK");
A.Classes["CL_ABAP_CONTEXT_INFO"].get_system_date = async () => {
  const d = new Date();
  const s = `${d.getFullYear()}${String(d.getMonth() + 1).padStart(2, "0")}${String(d.getDate()).padStart(2, "0")}`;
  return new A.types.Date({ qualifiedName: "D" }).set(s);
};
A.Classes["CL_ABAP_CONTEXT_INFO"].get_system_time = async () => {
  const d = new Date();
  const s = `${String(d.getHours()).padStart(2, "0")}${String(d.getMinutes()).padStart(2, "0")}${String(d.getSeconds()).padStart(2, "0")}`;
  return new A.types.Time({ qualifiedName: "T" }).set(s);
};

// ---------- BALI item setters ----------
const makeItem = (severity, text) =>
  obj("IF_BALI_ITEM_SETTER", { mock_severity: plain(severity), mock_text: plain(text) });

A.Classes["CL_BALI_MESSAGE_SETTER"].create = async (i) => makeItem(i.severity, i.number);
A.Classes["CL_BALI_MESSAGE_SETTER"].create_from_bapiret2 = async (i) => {
  const m = i.message_data.get();
  return makeItem(m.type, m.number);
};
A.Classes["CL_BALI_MESSAGE_SETTER"].create_from_sy = async () => makeItem("", "");
A.Classes["CL_BALI_FREE_TEXT_SETTER"].create = async (i) => makeItem(i.severity, i.text);
A.Classes["CL_BALI_EXCEPTION_SETTER"].create = async (i) => makeItem(i.severity, "exception");

// ---------- CL_BALI_HEADER_SETTER ----------
// The real system validates the log object against BAL customizing.
// Off-stack we mimic it with a known-objects list, so negative tests behave.
const KNOWN_BAL_OBJECTS = ["", "Z_CLOUD_LOG_SAMPLE"];
A.Classes["CL_BALI_HEADER_SETTER"].create = async (i) => {
  if (!KNOWN_BAL_OBJECTS.includes(plain(i.object).trim())) {
    const exc = await new A.Classes["CX_BALI_RUNTIME"]().constructor_({});
    throw exc;
  }
  const p = {};
  p.if_bali_header_setter$set_expiry = async () => obj("IF_BALI_HEADER_SETTER", p);
  return obj("IF_BALI_HEADER_SETTER", p);
};

// ---------- CL_BALI_LOG ----------
A.Classes["CL_BALI_LOG"].create = async () => {
  const items = [];
  const p = {
    mock_items: items,
    if_bali_log$set_header: async () => undefined,
    if_bali_log$add_item: async (i) => { items.push(i.item); },
    if_bali_log$get_handle: async () => chr(22, "MOCKHANDLE0000000001"),
    if_bali_log$add_all_items_from_other_log: async (i) => {
      const other = i.source_log.get();
      if (other && other.mock_items) items.push(...other.mock_items);
    },
  };
  return obj("IF_BALI_LOG", p);
};

// ---------- CL_BALI_LOG_DB ----------
A.Classes["CL_BALI_LOG_DB"].get_instance = async () =>
  obj("IF_BALI_LOG_DB", {
    if_bali_log_db$save_log: async () => undefined,
    if_bali_log_db$delete_log: async () => undefined,
  });

// ---------- XCO_CP ----------
const xcoMessagePointer = (msg) => ({
  if_xco_message$get_text: async () => {
    const m = msg.get ? msg.get() : msg;
    const parts = [m.msgv1, m.msgv2, m.msgv3, m.msgv4].map(plain).map(s => s.trim()).filter(Boolean);
    return str(parts.length ? parts.join(" ") : `Message ${plain(m.msgid)} ${plain(m.msgno)}`);
  },
  if_xco_message$value: msg,
  value: msg,
});
A.Classes["XCO_CP"].message = async (i) => obj("IF_XCO_MESSAGE", xcoMessagePointer(i.is_message));
A.Classes["XCO_CP"].string = async (i) => obj("IF_XCO_STRING", { mock: plain(i.iv_value) });
A.Classes["XCO_CP"].uuid = async () => obj("IF_XCO_UUID", {
  if_xco_uuid$as: async () => {
    const f = new A.types.Structure({ value: chr(36, crypto.randomUUID()) });
    return obj("IF_XCO_UUID_FORMATTED", undefined), (() => { const o = new A.types.ABAPObject({qualifiedName:"X"}); o.set({ value: chr(36, crypto.randomUUID()) }); return o; })();
  },
});
// sy message (log_syst_add) — read from runtime sy fields
A.Classes["XCO_CP"].sy = obj("IF_XCO_CP_STD_SY", {
  if_xco_cp_std_sy$message: async () => {
    const sy = A.builtin.sy.get();
    const val = new A.types.Structure({
      msgid: chr(20, plain(sy.msgid)), msgno: chr(3, plain(sy.msgno)),
      msgty: chr(1, plain(sy.msgty) || "S"),
      msgv1: chr(50, plain(sy.msgv1)), msgv2: chr(50, plain(sy.msgv2)),
      msgv3: chr(50, plain(sy.msgv3)), msgv4: chr(50, plain(sy.msgv4)),
    });
    return obj("IF_XCO_MESSAGE", { if_xco_message$value: val, value: val });
  },
});

// ---------- XCO_CP_JSON ----------
const abapToJs = (v) => {
  if (v === undefined || v === null) return null;
  if (v.array) return v.array().map(abapToJs);
  if (v.get) {
    const g = v.get();
    if (g && typeof g === "object" && !(g instanceof Date)) {
      const out = {};
      for (const k of Object.keys(g)) out[k] = abapToJs(g[k]);
      return out;
    }
    return g;
  }
  return v;
};
A.Classes["XCO_CP_JSON"].data = obj("IF_XCO_CP_JSON_DATA_FACTORY", {
  if_xco_cp_json_data_factory$from_abap: async (i) =>
    obj("IF_XCO_CP_JSON_DATA", {
      if_xco_cp_json_data$to_string: async function () {
        return str(JSON.stringify(abapToJs(i.ia_abap)));
      },
    }),
});

// ---------- CL_ABAP_UNIT_ASSERT (steampunk stub shadows the real one) ----------
class AssertError extends Error {}
const msgOf = (i) => (i && i.msg ? plain(i.msg) : "assertion failed");
const UA = A.Classes["CL_ABAP_UNIT_ASSERT"];
UA.fail = async (i) => { throw new AssertError(msgOf(i)); };
UA.assert_true = async (i) => { if (plain(i.act) !== "X") throw new AssertError(msgOf(i)); };
UA.assert_false = async (i) => { if (plain(i.act) === "X") throw new AssertError(msgOf(i)); };
UA.assert_bound = async (i) => { if (!i.act || i.act.get() === undefined) throw new AssertError(msgOf(i)); };
UA.assert_not_initial = async (i) => {
  if (A.compare.initial(i.act)) throw new AssertError(msgOf(i));
};
UA.assert_initial = async (i) => {
  if (!A.compare.initial(i.act)) throw new AssertError(msgOf(i));
};
UA.assert_equals = async (i) => {
  if (!A.compare.eq(i.act, i.exp)) {
    throw new AssertError(`${msgOf(i)} | exp=${plain(i.exp)} act=${plain(i.act)}`);
  }
};
UA.assert_char_cp = async (i) => {
  if (!A.compare.cp(i.act, i.exp)) {
    throw new AssertError(`${msgOf(i)} | pattern=${plain(i.exp)} act=${plain(i.act).slice(0, 60)}`);
  }
};

// ---------- CL_ABAP_TSTMP ----------
A.Classes["CL_ABAP_TSTMP"].subtract = async (i) => {
  const secs = Number(plain(i.tstmp1)) - Number(plain(i.tstmp2));
  return new A.types.Packed({ length: 8, decimals: 3 }).set(secs.toFixed(3));
};

// ---------- tolerant runner ----------
import { ltcl_external_methods } from "./output/zcl_cloud_logger.clas.testclasses.mjs";
const methods = ["test_instantiation","add_messages","create_and_save_log","create_wrong_log","empty_log",
  "log_contain_messages","merge_logs","check_error_in_log","check_warning_in_log","search_message_found",
  "search_message_not_found","search_message_with_type","test_fluent_chaining_1","free_and_reset_log",
  "test_log_data_as_json","use_same_instance","handle_not_initial","bapiret2_smart_filtering","test_timer",
  "test_sticky_context","get_instance_same_params","get_instance_omitted_params","get_instance_conflict_db_save",
  "get_instance_conflict_expiry","long_string_not_truncated","long_exception_not_trunc",
  "context_applies_to_all_methods","save_with_db_save_disabled","double_start_timer_warns",
  "save_in_loop_no_pollution","trim_limit_respects_custom","trim_limit_invalid_raises",
  "chain_initial_bapiret2_safe","chain_unbound_handle_safe","get_instance_conflict_trim"];

let pass = 0, fail = 0;
for (const m of methods) {
  const test = await (new ltcl_external_methods()).constructor_();
  const inst = test.FRIENDS_ACCESS_INSTANCE;
  try {
    if (inst.setup) await inst.setup();
    await inst[m]();
    if (inst.teardown) await inst.teardown();
    console.log(`  PASS  ${m}`);
    pass++;
  } catch (e) {
    console.log(`  FAIL  ${m}  (${(e && e.message ? e.message : e).toString().split("\n")[0].slice(0, 90)})`);
    try { if (inst.teardown) await inst.teardown(); } catch { /* ignore */ }
    fail++;
  }
}
console.log(`\nABAP Unit (off-stack): ${pass} passed, ${fail} failed, ${methods.length} total`);
process.exit(fail > 0 ? 1 : 0);
