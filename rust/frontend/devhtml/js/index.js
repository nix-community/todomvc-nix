
(function(l, r) { if (l.getElementById('livereloadscript')) return; r = l.createElement('script'); r.async = 1; r.src = '//' + (window.location.host || 'localhost').split(':')[0] + ':35729/livereload.js?snipver=1'; r.id = 'livereloadscript'; l.getElementsByTagName('head')[0].appendChild(r) })(window.document);
(function () {
    'use strict';

    function set_property(obj, name, value) { obj[name] = value; }

        function add_event(elem, name, f) {
            elem.addEventListener(name, f, {
                capture: true,
                once: false,
                passive: true
            });
        }

        function add_event_preventable(elem, name, f) {
            elem.addEventListener(name, f, {
                capture: true,
                once: false,
                passive: false
            });
        }

        function remove_event(elem, name, f) {
            elem.removeEventListener(name, f, true);
        }

    let wasm;

    const heap = new Array(32).fill(undefined);

    heap.push(undefined, null, true, false);

    function getObject(idx) { return heap[idx]; }

    let WASM_VECTOR_LEN = 0;

    let cachegetUint8Memory0 = null;
    function getUint8Memory0() {
        if (cachegetUint8Memory0 === null || cachegetUint8Memory0.buffer !== wasm.memory.buffer) {
            cachegetUint8Memory0 = new Uint8Array(wasm.memory.buffer);
        }
        return cachegetUint8Memory0;
    }

    let cachedTextEncoder = new TextEncoder('utf-8');

    const encodeString = (typeof cachedTextEncoder.encodeInto === 'function'
        ? function (arg, view) {
        return cachedTextEncoder.encodeInto(arg, view);
    }
        : function (arg, view) {
        const buf = cachedTextEncoder.encode(arg);
        view.set(buf);
        return {
            read: arg.length,
            written: buf.length
        };
    });

    function passStringToWasm0(arg, malloc, realloc) {

        if (typeof(arg) !== 'string') throw new Error('expected a string argument');

        if (realloc === undefined) {
            const buf = cachedTextEncoder.encode(arg);
            const ptr = malloc(buf.length);
            getUint8Memory0().subarray(ptr, ptr + buf.length).set(buf);
            WASM_VECTOR_LEN = buf.length;
            return ptr;
        }

        let len = arg.length;
        let ptr = malloc(len);

        const mem = getUint8Memory0();

        let offset = 0;

        for (; offset < len; offset++) {
            const code = arg.charCodeAt(offset);
            if (code > 0x7F) break;
            mem[ptr + offset] = code;
        }

        if (offset !== len) {
            if (offset !== 0) {
                arg = arg.slice(offset);
            }
            ptr = realloc(ptr, len, len = offset + arg.length * 3);
            const view = getUint8Memory0().subarray(ptr + offset, ptr + len);
            const ret = encodeString(arg, view);
            if (ret.read !== arg.length) throw new Error('failed to pass whole string');
            offset += ret.written;
        }

        WASM_VECTOR_LEN = offset;
        return ptr;
    }

    let cachegetInt32Memory0 = null;
    function getInt32Memory0() {
        if (cachegetInt32Memory0 === null || cachegetInt32Memory0.buffer !== wasm.memory.buffer) {
            cachegetInt32Memory0 = new Int32Array(wasm.memory.buffer);
        }
        return cachegetInt32Memory0;
    }

    let cachedTextDecoder = new TextDecoder('utf-8', { ignoreBOM: true, fatal: true });

    cachedTextDecoder.decode();

    function getStringFromWasm0(ptr, len) {
        return cachedTextDecoder.decode(getUint8Memory0().subarray(ptr, ptr + len));
    }

    let heap_next = heap.length;

    function addHeapObject(obj) {
        if (heap_next === heap.length) heap.push(heap.length + 1);
        const idx = heap_next;
        heap_next = heap[idx];

        if (typeof(heap_next) !== 'number') throw new Error('corrupt heap');

        heap[idx] = obj;
        return idx;
    }

    function dropObject(idx) {
        if (idx < 36) return;
        heap[idx] = heap_next;
        heap_next = idx;
    }

    function takeObject(idx) {
        const ret = getObject(idx);
        dropObject(idx);
        return ret;
    }

    function _assertBoolean(n) {
        if (typeof(n) !== 'boolean') {
            throw new Error('expected a boolean argument');
        }
    }

    function makeMutClosure(arg0, arg1, dtor, f) {
        const state = { a: arg0, b: arg1, cnt: 1, dtor };
        const real = (...args) => {
            // First up with a closure we increment the internal reference
            // count. This ensures that the Rust closure environment won't
            // be deallocated while we're invoking it.
            state.cnt++;
            const a = state.a;
            state.a = 0;
            try {
                return f(a, state.b, ...args);
            } finally {
                if (--state.cnt === 0) {
                    wasm.__wbindgen_export_2.get(state.dtor)(a, state.b);

                } else {
                    state.a = a;
                }
            }
        };
        real.original = state;

        return real;
    }

    function logError(f) {
        return function () {
            try {
                return f.apply(this, arguments);

            } catch (e) {
                let error = (function () {
                    try {
                        return e instanceof Error ? `${e.message}\n\nStack:\n${e.stack}` : e.toString();
                    } catch(_) {
                        return "<failed to stringify thrown value>";
                    }
                }());
                console.error("wasm-bindgen: imported JS function that was not marked as `catch` threw an error:", error);
                throw e;
            }
        };
    }

    function _assertNum(n) {
        if (typeof(n) !== 'number') throw new Error('expected a number argument');
    }

    let stack_pointer = 32;

    function addBorrowedObject(obj) {
        if (stack_pointer == 1) throw new Error('out of js stack');
        heap[--stack_pointer] = obj;
        return stack_pointer;
    }
    function __wbg_adapter_18(arg0, arg1, arg2) {
        try {
            _assertNum(arg0);
            _assertNum(arg1);
            wasm._dyn_core__ops__function__FnMut___A____Output___R_as_wasm_bindgen__closure__WasmClosure___describe__invoke__h3bba74ed4e0776fa(arg0, arg1, addBorrowedObject(arg2));
        } finally {
            heap[stack_pointer++] = undefined;
        }
    }

    function __wbg_adapter_21(arg0, arg1, arg2) {
        _assertNum(arg0);
        _assertNum(arg1);
        wasm._dyn_core__ops__function__FnMut__A____Output___R_as_wasm_bindgen__closure__WasmClosure___describe__invoke__h76466894b7a410fa(arg0, arg1, addHeapObject(arg2));
    }

    function getCachedStringFromWasm0(ptr, len) {
        if (ptr === 0) {
            return getObject(len);
        } else {
            return getStringFromWasm0(ptr, len);
        }
    }

    function isLikeNone(x) {
        return x === undefined || x === null;
    }

    function handleError(f) {
        return function () {
            try {
                return f.apply(this, arguments);

            } catch (e) {
                wasm.__wbindgen_exn_store(addHeapObject(e));
            }
        };
    }

    async function load(module, imports) {
        if (typeof Response === 'function' && module instanceof Response) {

            if (typeof WebAssembly.instantiateStreaming === 'function') {
                try {
                    return await WebAssembly.instantiateStreaming(module, imports);

                } catch (e) {
                    if (module.headers.get('Content-Type') != 'application/wasm') {
                        console.warn("`WebAssembly.instantiateStreaming` failed because your server does not serve wasm with `application/wasm` MIME type. Falling back to `WebAssembly.instantiate` which is slower. Original error:\n", e);

                    } else {
                        throw e;
                    }
                }
            }

            const bytes = await module.arrayBuffer();
            return await WebAssembly.instantiate(bytes, imports);

        } else {

            const instance = await WebAssembly.instantiate(module, imports);

            if (instance instanceof WebAssembly.Instance) {
                return { instance, module };

            } else {
                return instance;
            }
        }
    }

    async function init(input) {
        if (typeof input === 'undefined') {
            input = (document.currentScript && document.currentScript.src || new URL('index.js', document.baseURI).href).replace(/\.js$/, '_bg.wasm');
        }
        const imports = {};
        imports.wbg = {};
        imports.wbg.__wbindgen_json_serialize = function(arg0, arg1) {
            const obj = getObject(arg1);
            var ret = JSON.stringify(obj === undefined ? null : obj);
            var ptr0 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
            var len0 = WASM_VECTOR_LEN;
            getInt32Memory0()[arg0 / 4 + 1] = len0;
            getInt32Memory0()[arg0 / 4 + 0] = ptr0;
        };
        imports.wbg.__wbindgen_string_new = function(arg0, arg1) {
            var ret = getStringFromWasm0(arg0, arg1);
            return addHeapObject(ret);
        };
        imports.wbg.__wbindgen_object_clone_ref = function(arg0) {
            var ret = getObject(arg0);
            return addHeapObject(ret);
        };
        imports.wbg.__wbg_error_4bb6c2a97407129a = logError(function(arg0, arg1) {
            var v0 = getCachedStringFromWasm0(arg0, arg1);
        if (arg0 !== 0) { wasm.__wbindgen_free(arg0, arg1); }
        console.error(v0);
    });
    imports.wbg.__wbg_new_59cb74e423758ede = logError(function() {
        var ret = new Error();
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_stack_558ba5917b466edd = logError(function(arg0, arg1) {
        var ret = getObject(arg1).stack;
        var ptr0 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len0 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len0;
        getInt32Memory0()[arg0 / 4 + 0] = ptr0;
    });
    imports.wbg.__wbindgen_cb_drop = function(arg0) {
        const obj = takeObject(arg0).original;
        if (obj.cnt-- == 1) {
            obj.a = 0;
            return true;
        }
        var ret = false;
        _assertBoolean(ret);
        return ret;
    };
    imports.wbg.__wbg_setproperty_63cd02ca3f62dac4 = logError(function(arg0, arg1, arg2, arg3) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        set_property(getObject(arg0), v0, getObject(arg3));
    });
    imports.wbg.__wbg_addevent_ccc6ab519df88e22 = logError(function(arg0, arg1, arg2, arg3) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        add_event(getObject(arg0), v0, getObject(arg3));
    });
    imports.wbg.__wbg_addeventpreventable_727fdc15bb54071b = logError(function(arg0, arg1, arg2, arg3) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        add_event_preventable(getObject(arg0), v0, getObject(arg3));
    });
    imports.wbg.__wbg_removeevent_0f7fcdc74d4840b2 = logError(function(arg0, arg1, arg2, arg3) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        remove_event(getObject(arg0), v0, getObject(arg3));
    });
    imports.wbg.__wbg_instanceof_Window_49f532f06a9786ee = logError(function(arg0) {
        var ret = getObject(arg0) instanceof Window;
        _assertBoolean(ret);
        return ret;
    });
    imports.wbg.__wbg_document_c0366b39e4f4c89a = logError(function(arg0) {
        var ret = getObject(arg0).document;
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    });
    imports.wbg.__wbg_location_c1e50a6e4c53d45c = logError(function(arg0) {
        var ret = getObject(arg0).location;
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_history_eba22183a08cc7a7 = handleError(function(arg0) {
        var ret = getObject(arg0).history;
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_fetch_29be7ecdff099463 = logError(function(arg0, arg1, arg2, arg3) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        var ret = getObject(arg0).fetch(v0, getObject(arg3));
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_head_e6012e4b573fe595 = logError(function(arg0) {
        var ret = getObject(arg0).head;
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    });
    imports.wbg.__wbg_createElement_99351c8bf0efac6e = handleError(function(arg0, arg1, arg2) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        var ret = getObject(arg0).createElement(v0);
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_createTextNode_cfdcc8da0d55d336 = logError(function(arg0, arg1, arg2) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        var ret = getObject(arg0).createTextNode(v0);
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_getElementById_15aef17a620252b4 = logError(function(arg0, arg1, arg2) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        var ret = getObject(arg0).getElementById(v0);
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    });
    imports.wbg.__wbg_ok_c20643e0a45dc5a0 = logError(function(arg0) {
        var ret = getObject(arg0).ok;
        _assertBoolean(ret);
        return ret;
    });
    imports.wbg.__wbg_json_012a7a84489a5ec5 = handleError(function(arg0) {
        var ret = getObject(arg0).json();
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_hash_43b2bf44e856f329 = logError(function(arg0, arg1) {
        var ret = getObject(arg1).hash;
        var ptr0 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len0 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len0;
        getInt32Memory0()[arg0 / 4 + 0] = ptr0;
    });
    imports.wbg.__wbg_new_bcd64c6565e8d34f = handleError(function(arg0, arg1) {
        var v0 = getCachedStringFromWasm0(arg0, arg1);
        var ret = new URL(v0);
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_instanceof_HtmlInputElement_ad83b145c236a35b = logError(function(arg0) {
        var ret = getObject(arg0) instanceof HTMLInputElement;
        _assertBoolean(ret);
        return ret;
    });
    imports.wbg.__wbg_checked_6fa4ca4dbb11badf = logError(function(arg0) {
        var ret = getObject(arg0).checked;
        _assertBoolean(ret);
        return ret;
    });
    imports.wbg.__wbg_type_a221eb70933ab026 = logError(function(arg0, arg1) {
        var ret = getObject(arg1).type;
        var ptr0 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len0 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len0;
        getInt32Memory0()[arg0 / 4 + 0] = ptr0;
    });
    imports.wbg.__wbg_value_97fba2fa96f7251f = logError(function(arg0, arg1) {
        var ret = getObject(arg1).value;
        var ptr0 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len0 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len0;
        getInt32Memory0()[arg0 / 4 + 0] = ptr0;
    });
    imports.wbg.__wbg_length_5f1555ec686a5fb2 = logError(function(arg0) {
        var ret = getObject(arg0).length;
        _assertNum(ret);
        return ret;
    });
    imports.wbg.__wbg_get_890e1f19927be4f7 = logError(function(arg0, arg1) {
        var ret = getObject(arg0)[arg1 >>> 0];
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    });
    imports.wbg.__wbg_style_418c8723fb000d37 = logError(function(arg0) {
        var ret = getObject(arg0).style;
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_cssRules_578a368f52c54b9c = handleError(function(arg0) {
        var ret = getObject(arg0).cssRules;
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_insertRule_78629c7bca1717ae = handleError(function(arg0, arg1, arg2, arg3) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        var ret = getObject(arg0).insertRule(v0, arg3 >>> 0);
        _assertNum(ret);
        return ret;
    });
    imports.wbg.__wbg_target_4bc4eb28204bcc44 = logError(function(arg0) {
        var ret = getObject(arg0).target;
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    });
    imports.wbg.__wbg_preventDefault_9aab6c264e5df3ee = logError(function(arg0) {
        getObject(arg0).preventDefault();
    });
    imports.wbg.__wbg_key_d9b602f48baca7bc = logError(function(arg0, arg1) {
        var ret = getObject(arg1).key;
        var ptr0 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len0 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len0;
        getInt32Memory0()[arg0 / 4 + 0] = ptr0;
    });
    imports.wbg.__wbg_pushState_7bfc468a9d1367a3 = handleError(function(arg0, arg1, arg2, arg3, arg4, arg5) {
        var v0 = getCachedStringFromWasm0(arg2, arg3);
        var v1 = getCachedStringFromWasm0(arg4, arg5);
        getObject(arg0).pushState(getObject(arg1), v0, v1);
    });
    imports.wbg.__wbg_appendChild_7c45aeccd496f2a5 = handleError(function(arg0, arg1) {
        var ret = getObject(arg0).appendChild(getObject(arg1));
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_insertBefore_6e8e209ea019870f = handleError(function(arg0, arg1, arg2) {
        var ret = getObject(arg0).insertBefore(getObject(arg1), getObject(arg2));
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_removeChild_1e1942a296b255c1 = handleError(function(arg0, arg1) {
        var ret = getObject(arg0).removeChild(getObject(arg1));
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_replaceChild_a3d4a6fb76155a55 = handleError(function(arg0, arg1, arg2) {
        var ret = getObject(arg0).replaceChild(getObject(arg1), getObject(arg2));
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_classList_3cb76044e9478d02 = logError(function(arg0) {
        var ret = getObject(arg0).classList;
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_setAttribute_e71b9086539f06a1 = handleError(function(arg0, arg1, arg2, arg3, arg4) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        var v1 = getCachedStringFromWasm0(arg3, arg4);
        getObject(arg0).setAttribute(v0, v1);
    });
    imports.wbg.__wbg_debug_9f067aefe2ceaadd = logError(function(arg0, arg1, arg2, arg3) {
        console.debug(getObject(arg0), getObject(arg1), getObject(arg2), getObject(arg3));
    });
    imports.wbg.__wbg_error_e325755affc8634b = logError(function(arg0) {
        console.error(getObject(arg0));
    });
    imports.wbg.__wbg_error_7bb15b842d5b0ddb = logError(function(arg0, arg1, arg2, arg3) {
        console.error(getObject(arg0), getObject(arg1), getObject(arg2), getObject(arg3));
    });
    imports.wbg.__wbg_info_1b9fdabaafc8f4cb = logError(function(arg0, arg1, arg2, arg3) {
        console.info(getObject(arg0), getObject(arg1), getObject(arg2), getObject(arg3));
    });
    imports.wbg.__wbg_log_37120b26fb738792 = logError(function(arg0, arg1, arg2, arg3) {
        console.log(getObject(arg0), getObject(arg1), getObject(arg2), getObject(arg3));
    });
    imports.wbg.__wbg_warn_6add4f04160cdbba = logError(function(arg0, arg1, arg2, arg3) {
        console.warn(getObject(arg0), getObject(arg1), getObject(arg2), getObject(arg3));
    });
    imports.wbg.__wbg_instanceof_HtmlElement_ed44c8f443dbd619 = logError(function(arg0) {
        var ret = getObject(arg0) instanceof HTMLElement;
        _assertBoolean(ret);
        return ret;
    });
    imports.wbg.__wbg_blur_252cc433057fcc5d = handleError(function(arg0) {
        getObject(arg0).blur();
    });
    imports.wbg.__wbg_focus_537b5fc3e87a7c39 = handleError(function(arg0) {
        getObject(arg0).focus();
    });
    imports.wbg.__wbg_setdata_5d781dc19c0eb4ba = logError(function(arg0, arg1, arg2) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        getObject(arg0).data = v0;
    });
    imports.wbg.__wbg_getPropertyValue_51b3e4d3afc1266c = handleError(function(arg0, arg1, arg2, arg3) {
        var v0 = getCachedStringFromWasm0(arg2, arg3);
        var ret = getObject(arg1).getPropertyValue(v0);
        var ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    });
    imports.wbg.__wbg_removeProperty_dfad019c7011a427 = handleError(function(arg0, arg1, arg2, arg3) {
        var v0 = getCachedStringFromWasm0(arg2, arg3);
        var ret = getObject(arg1).removeProperty(v0);
        var ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len1 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len1;
        getInt32Memory0()[arg0 / 4 + 0] = ptr1;
    });
    imports.wbg.__wbg_setProperty_c4294d008ae89f52 = handleError(function(arg0, arg1, arg2, arg3, arg4, arg5, arg6) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        var v1 = getCachedStringFromWasm0(arg3, arg4);
        var v2 = getCachedStringFromWasm0(arg5, arg6);
        getObject(arg0).setProperty(v0, v1, v2);
    });
    imports.wbg.__wbg_href_334d59175e10cf4f = handleError(function(arg0, arg1) {
        var ret = getObject(arg1).href;
        var ptr0 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len0 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len0;
        getInt32Memory0()[arg0 / 4 + 0] = ptr0;
    });
    imports.wbg.__wbg_add_81e93fbfffcb5553 = handleError(function(arg0, arg1, arg2) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        getObject(arg0).add(v0);
    });
    imports.wbg.__wbg_remove_e110e68de3bca52b = handleError(function(arg0, arg1, arg2) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        getObject(arg0).remove(v0);
    });
    imports.wbg.__wbg_new_83a0b608494484fd = handleError(function() {
        var ret = new Headers();
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_append_6363bf15ad177fce = handleError(function(arg0, arg1, arg2, arg3, arg4) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        var v1 = getCachedStringFromWasm0(arg3, arg4);
        getObject(arg0).append(v0, v1);
    });
    imports.wbg.__wbg_set_76c6c83392bce553 = handleError(function(arg0, arg1, arg2, arg3, arg4) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        var v1 = getCachedStringFromWasm0(arg3, arg4);
        getObject(arg0).set(v0, v1);
    });
    imports.wbg.__wbg_settype_0cef614b60cb523d = logError(function(arg0, arg1, arg2) {
        var v0 = getCachedStringFromWasm0(arg1, arg2);
        getObject(arg0).type = v0;
    });
    imports.wbg.__wbg_sheet_f9ea9628bf990cb6 = logError(function(arg0) {
        var ret = getObject(arg0).sheet;
        return isLikeNone(ret) ? 0 : addHeapObject(ret);
    });
    imports.wbg.__wbg_instanceof_HtmlTextAreaElement_aa81cb6ef637ad1f = logError(function(arg0) {
        var ret = getObject(arg0) instanceof HTMLTextAreaElement;
        _assertBoolean(ret);
        return ret;
    });
    imports.wbg.__wbg_value_0938d95709a8299e = logError(function(arg0, arg1) {
        var ret = getObject(arg1).value;
        var ptr0 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len0 = WASM_VECTOR_LEN;
        getInt32Memory0()[arg0 / 4 + 1] = len0;
        getInt32Memory0()[arg0 / 4 + 0] = ptr0;
    });
    imports.wbg.__wbg_new_94a7dfa9529ec6e8 = logError(function(arg0, arg1) {
        var v0 = getCachedStringFromWasm0(arg0, arg1);
        var ret = new Error(v0);
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_newnoargs_7c6bd521992b4022 = logError(function(arg0, arg1) {
        var v0 = getCachedStringFromWasm0(arg0, arg1);
        var ret = new Function(v0);
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_call_951bd0c6d815d6f1 = handleError(function(arg0, arg1) {
        var ret = getObject(arg0).call(getObject(arg1));
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_new_ba07d0daa0e4677e = logError(function() {
        var ret = new Object();
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_resolve_6e61e640925a0db9 = logError(function(arg0) {
        var ret = Promise.resolve(getObject(arg0));
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_then_dd3785597974798a = logError(function(arg0, arg1) {
        var ret = getObject(arg0).then(getObject(arg1));
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_then_0f957e0f4c3e537a = logError(function(arg0, arg1, arg2) {
        var ret = getObject(arg0).then(getObject(arg1), getObject(arg2));
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_globalThis_513fb247e8e4e6d2 = handleError(function() {
        var ret = globalThis.globalThis;
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_self_6baf3a3aa7b63415 = handleError(function() {
        var ret = self.self;
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_window_63fc4027b66c265b = handleError(function() {
        var ret = window.window;
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_global_b87245cd886d7113 = handleError(function() {
        var ret = global.global;
        return addHeapObject(ret);
    });
    imports.wbg.__wbg_set_9bdd413385146137 = handleError(function(arg0, arg1, arg2) {
        var ret = Reflect.set(getObject(arg0), getObject(arg1), getObject(arg2));
        _assertBoolean(ret);
        return ret;
    });
    imports.wbg.__wbindgen_is_undefined = function(arg0) {
        var ret = getObject(arg0) === undefined;
        _assertBoolean(ret);
        return ret;
    };
    imports.wbg.__wbindgen_object_drop_ref = function(arg0) {
        takeObject(arg0);
    };
    imports.wbg.__wbindgen_throw = function(arg0, arg1) {
        throw new Error(getStringFromWasm0(arg0, arg1));
    };
    imports.wbg.__wbindgen_rethrow = function(arg0) {
        throw takeObject(arg0);
    };
    imports.wbg.__wbindgen_closure_wrapper4068 = logError(function(arg0, arg1, arg2) {
        var ret = makeMutClosure(arg0, arg1, 164, __wbg_adapter_18);
        return addHeapObject(ret);
    });
    imports.wbg.__wbindgen_closure_wrapper4912 = logError(function(arg0, arg1, arg2) {
        var ret = makeMutClosure(arg0, arg1, 199, __wbg_adapter_21);
        return addHeapObject(ret);
    });

    if (typeof input === 'string' || (typeof Request === 'function' && input instanceof Request) || (typeof URL === 'function' && input instanceof URL)) {
        input = fetch(input);
    }

    const { instance, module } = await load(await input, imports);

    wasm = instance.exports;
    init.__wbindgen_wasm_module = module;
    wasm.__wbindgen_start();
    return wasm;
    }

    init("/js/assets/rust-frontend.wasm").catch(console.error);

}());
//# sourceMappingURL=index.js.map
