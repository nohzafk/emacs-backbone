// @bun
// build/dev/javascript/prelude.mjs
class CustomType {
  withFields(fields) {
    let properties = Object.keys(this).map((label) => (label in fields) ? fields[label] : this[label]);
    return new this.constructor(...properties);
  }
}

class List {
  static fromArray(array, tail) {
    let t = tail || new Empty;
    for (let i = array.length - 1;i >= 0; --i) {
      t = new NonEmpty(array[i], t);
    }
    return t;
  }
  [Symbol.iterator]() {
    return new ListIterator(this);
  }
  toArray() {
    return [...this];
  }
  atLeastLength(desired) {
    let current = this;
    while (desired-- > 0 && current)
      current = current.tail;
    return current !== undefined;
  }
  hasLength(desired) {
    let current = this;
    while (desired-- > 0 && current)
      current = current.tail;
    return desired === -1 && current instanceof Empty;
  }
  countLength() {
    let current = this;
    let length = 0;
    while (current) {
      current = current.tail;
      length++;
    }
    return length - 1;
  }
}
function prepend(element, tail) {
  return new NonEmpty(element, tail);
}
function toList(elements, tail) {
  return List.fromArray(elements, tail);
}

class ListIterator {
  #current;
  constructor(current) {
    this.#current = current;
  }
  next() {
    if (this.#current instanceof Empty) {
      return { done: true };
    } else {
      let { head, tail } = this.#current;
      this.#current = tail;
      return { value: head, done: false };
    }
  }
}

class Empty extends List {
}
class NonEmpty extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
}
class BitArray {
  bitSize;
  byteSize;
  bitOffset;
  rawBuffer;
  constructor(buffer, bitSize, bitOffset) {
    if (!(buffer instanceof Uint8Array)) {
      throw globalThis.Error("BitArray can only be constructed from a Uint8Array");
    }
    this.bitSize = bitSize ?? buffer.length * 8;
    this.byteSize = Math.trunc((this.bitSize + 7) / 8);
    this.bitOffset = bitOffset ?? 0;
    if (this.bitSize < 0) {
      throw globalThis.Error(`BitArray bit size is invalid: ${this.bitSize}`);
    }
    if (this.bitOffset < 0 || this.bitOffset > 7) {
      throw globalThis.Error(`BitArray bit offset is invalid: ${this.bitOffset}`);
    }
    if (buffer.length !== Math.trunc((this.bitOffset + this.bitSize + 7) / 8)) {
      throw globalThis.Error("BitArray buffer length is invalid");
    }
    this.rawBuffer = buffer;
  }
  byteAt(index) {
    if (index < 0 || index >= this.byteSize) {
      return;
    }
    return bitArrayByteAt(this.rawBuffer, this.bitOffset, index);
  }
  equals(other) {
    if (this.bitSize !== other.bitSize) {
      return false;
    }
    const wholeByteCount = Math.trunc(this.bitSize / 8);
    if (this.bitOffset === 0 && other.bitOffset === 0) {
      for (let i = 0;i < wholeByteCount; i++) {
        if (this.rawBuffer[i] !== other.rawBuffer[i]) {
          return false;
        }
      }
      const trailingBitsCount = this.bitSize % 8;
      if (trailingBitsCount) {
        const unusedLowBitCount = 8 - trailingBitsCount;
        if (this.rawBuffer[wholeByteCount] >> unusedLowBitCount !== other.rawBuffer[wholeByteCount] >> unusedLowBitCount) {
          return false;
        }
      }
    } else {
      for (let i = 0;i < wholeByteCount; i++) {
        const a = bitArrayByteAt(this.rawBuffer, this.bitOffset, i);
        const b = bitArrayByteAt(other.rawBuffer, other.bitOffset, i);
        if (a !== b) {
          return false;
        }
      }
      const trailingBitsCount = this.bitSize % 8;
      if (trailingBitsCount) {
        const a = bitArrayByteAt(this.rawBuffer, this.bitOffset, wholeByteCount);
        const b = bitArrayByteAt(other.rawBuffer, other.bitOffset, wholeByteCount);
        const unusedLowBitCount = 8 - trailingBitsCount;
        if (a >> unusedLowBitCount !== b >> unusedLowBitCount) {
          return false;
        }
      }
    }
    return true;
  }
  get buffer() {
    if (this.bitOffset !== 0 || this.bitSize % 8 !== 0) {
      throw new globalThis.Error("BitArray.buffer does not support unaligned bit arrays");
    }
    return this.rawBuffer;
  }
  get length() {
    if (this.bitOffset !== 0 || this.bitSize % 8 !== 0) {
      throw new globalThis.Error("BitArray.length does not support unaligned bit arrays");
    }
    return this.rawBuffer.length;
  }
}
function bitArrayByteAt(buffer, bitOffset, index) {
  if (bitOffset === 0) {
    return buffer[index] ?? 0;
  } else {
    const a = buffer[index] << bitOffset & 255;
    const b = buffer[index + 1] >> 8 - bitOffset;
    return a | b;
  }
}

class UtfCodepoint {
  constructor(value) {
    this.value = value;
  }
}
function toBitArray(segments) {
  if (segments.length === 0) {
    return new BitArray(new Uint8Array);
  }
  if (segments.length === 1) {
    const segment = segments[0];
    if (segment instanceof BitArray) {
      return segment;
    }
    if (segment instanceof Uint8Array) {
      return new BitArray(segment);
    }
    return new BitArray(new Uint8Array(segments));
  }
  let bitSize = 0;
  let areAllSegmentsNumbers = true;
  for (const segment of segments) {
    if (segment instanceof BitArray) {
      bitSize += segment.bitSize;
      areAllSegmentsNumbers = false;
    } else if (segment instanceof Uint8Array) {
      bitSize += segment.byteLength * 8;
      areAllSegmentsNumbers = false;
    } else {
      bitSize += 8;
    }
  }
  if (areAllSegmentsNumbers) {
    return new BitArray(new Uint8Array(segments));
  }
  const buffer = new Uint8Array(Math.trunc((bitSize + 7) / 8));
  let cursor = 0;
  for (let segment of segments) {
    const isCursorByteAligned = cursor % 8 === 0;
    if (segment instanceof BitArray) {
      if (isCursorByteAligned && segment.bitOffset === 0) {
        buffer.set(segment.rawBuffer, cursor / 8);
        cursor += segment.bitSize;
        const trailingBitsCount = segment.bitSize % 8;
        if (trailingBitsCount !== 0) {
          const lastByteIndex = Math.trunc(cursor / 8);
          buffer[lastByteIndex] >>= 8 - trailingBitsCount;
          buffer[lastByteIndex] <<= 8 - trailingBitsCount;
        }
      } else {
        appendUnalignedBits(segment.rawBuffer, segment.bitSize, segment.bitOffset);
      }
    } else if (segment instanceof Uint8Array) {
      if (isCursorByteAligned) {
        buffer.set(segment, cursor / 8);
        cursor += segment.byteLength * 8;
      } else {
        appendUnalignedBits(segment, segment.byteLength * 8, 0);
      }
    } else {
      if (isCursorByteAligned) {
        buffer[cursor / 8] = segment;
        cursor += 8;
      } else {
        appendUnalignedBits(new Uint8Array([segment]), 8, 0);
      }
    }
  }
  function appendUnalignedBits(unalignedBits, size, offset) {
    if (size === 0) {
      return;
    }
    const byteSize = Math.trunc(size + 7 / 8);
    const highBitsCount = cursor % 8;
    const lowBitsCount = 8 - highBitsCount;
    let byteIndex = Math.trunc(cursor / 8);
    for (let i = 0;i < byteSize; i++) {
      let byte = bitArrayByteAt(unalignedBits, offset, i);
      if (size < 8) {
        byte >>= 8 - size;
        byte <<= 8 - size;
      }
      buffer[byteIndex] |= byte >> highBitsCount;
      let appendedBitsCount = size - Math.max(0, size - lowBitsCount);
      size -= appendedBitsCount;
      cursor += appendedBitsCount;
      if (size === 0) {
        break;
      }
      buffer[++byteIndex] = byte << lowBitsCount;
      appendedBitsCount = size - Math.max(0, size - highBitsCount);
      size -= appendedBitsCount;
      cursor += appendedBitsCount;
    }
  }
  return new BitArray(buffer, bitSize);
}
var utf8Encoder;
function stringBits(string) {
  utf8Encoder ??= new TextEncoder;
  return utf8Encoder.encode(string);
}
class Result extends CustomType {
  static isResult(data) {
    return data instanceof Result;
  }
}

class Ok extends Result {
  constructor(value) {
    super();
    this[0] = value;
  }
  isOk() {
    return true;
  }
}
class Error extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }
  isOk() {
    return false;
  }
}
function isEqual(x, y) {
  let values = [x, y];
  while (values.length) {
    let a = values.pop();
    let b = values.pop();
    if (a === b)
      continue;
    if (!isObject(a) || !isObject(b))
      return false;
    let unequal = !structurallyCompatibleObjects(a, b) || unequalDates(a, b) || unequalBuffers(a, b) || unequalArrays(a, b) || unequalMaps(a, b) || unequalSets(a, b) || unequalRegExps(a, b);
    if (unequal)
      return false;
    const proto = Object.getPrototypeOf(a);
    if (proto !== null && typeof proto.equals === "function") {
      try {
        if (a.equals(b))
          continue;
        else
          return false;
      } catch {}
    }
    let [keys, get] = getters(a);
    const ka = keys(a);
    const kb = keys(b);
    if (ka.length !== kb.length)
      return false;
    for (let k of ka) {
      values.push(get(a, k), get(b, k));
    }
  }
  return true;
}
function getters(object) {
  if (object instanceof Map) {
    return [(x) => x.keys(), (x, y) => x.get(y)];
  } else {
    let extra = object instanceof globalThis.Error ? ["message"] : [];
    return [(x) => [...extra, ...Object.keys(x)], (x, y) => x[y]];
  }
}
function unequalDates(a, b) {
  return a instanceof Date && (a > b || a < b);
}
function unequalBuffers(a, b) {
  return !(a instanceof BitArray) && a.buffer instanceof ArrayBuffer && a.BYTES_PER_ELEMENT && !(a.byteLength === b.byteLength && a.every((n, i) => n === b[i]));
}
function unequalArrays(a, b) {
  return Array.isArray(a) && a.length !== b.length;
}
function unequalMaps(a, b) {
  return a instanceof Map && a.size !== b.size;
}
function unequalSets(a, b) {
  return a instanceof Set && (a.size != b.size || [...a].some((e) => !b.has(e)));
}
function unequalRegExps(a, b) {
  return a instanceof RegExp && (a.source !== b.source || a.flags !== b.flags);
}
function isObject(a) {
  return typeof a === "object" && a !== null;
}
function structurallyCompatibleObjects(a, b) {
  if (typeof a !== "object" && typeof b !== "object" && (!a || !b))
    return false;
  let nonstructural = [Promise, WeakSet, WeakMap, Function];
  if (nonstructural.some((c) => a instanceof c))
    return false;
  return a.constructor === b.constructor;
}
function remainderInt(a, b) {
  if (b === 0) {
    return 0;
  } else {
    return a % b;
  }
}
function divideInt(a, b) {
  return Math.trunc(divideFloat(a, b));
}
function divideFloat(a, b) {
  if (b === 0) {
    return 0;
  } else {
    return a / b;
  }
}
function makeError(variant, file, module, line, fn, message, extra) {
  let error = new globalThis.Error(message);
  error.gleam_error = variant;
  error.file = file;
  error.module = module;
  error.line = line;
  error.function = fn;
  error.fn = fn;
  for (let k in extra)
    error[k] = extra[k];
  return error;
}
// build/dev/javascript/gleam_stdlib/gleam/order.mjs
class Lt extends CustomType {
}
class Eq extends CustomType {
}
class Gt extends CustomType {
}

// build/dev/javascript/gleam_stdlib/gleam/option.mjs
class Some extends CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
class None extends CustomType {
}
function is_some(option) {
  return !(option instanceof None);
}
function unwrap(option, default$) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else {
    return default$;
  }
}

// build/dev/javascript/gleam_stdlib/dict.mjs
var referenceMap = /* @__PURE__ */ new WeakMap;
var tempDataView = /* @__PURE__ */ new DataView(/* @__PURE__ */ new ArrayBuffer(8));
var referenceUID = 0;
function hashByReference(o) {
  const known = referenceMap.get(o);
  if (known !== undefined) {
    return known;
  }
  const hash = referenceUID++;
  if (referenceUID === 2147483647) {
    referenceUID = 0;
  }
  referenceMap.set(o, hash);
  return hash;
}
function hashMerge(a, b) {
  return a ^ b + 2654435769 + (a << 6) + (a >> 2) | 0;
}
function hashString(s) {
  let hash = 0;
  const len = s.length;
  for (let i = 0;i < len; i++) {
    hash = Math.imul(31, hash) + s.charCodeAt(i) | 0;
  }
  return hash;
}
function hashNumber(n) {
  tempDataView.setFloat64(0, n);
  const i = tempDataView.getInt32(0);
  const j = tempDataView.getInt32(4);
  return Math.imul(73244475, i >> 16 ^ i) ^ j;
}
function hashBigInt(n) {
  return hashString(n.toString());
}
function hashObject(o) {
  const proto = Object.getPrototypeOf(o);
  if (proto !== null && typeof proto.hashCode === "function") {
    try {
      const code = o.hashCode(o);
      if (typeof code === "number") {
        return code;
      }
    } catch {}
  }
  if (o instanceof Promise || o instanceof WeakSet || o instanceof WeakMap) {
    return hashByReference(o);
  }
  if (o instanceof Date) {
    return hashNumber(o.getTime());
  }
  let h = 0;
  if (o instanceof ArrayBuffer) {
    o = new Uint8Array(o);
  }
  if (Array.isArray(o) || o instanceof Uint8Array) {
    for (let i = 0;i < o.length; i++) {
      h = Math.imul(31, h) + getHash(o[i]) | 0;
    }
  } else if (o instanceof Set) {
    o.forEach((v) => {
      h = h + getHash(v) | 0;
    });
  } else if (o instanceof Map) {
    o.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
  } else {
    const keys = Object.keys(o);
    for (let i = 0;i < keys.length; i++) {
      const k = keys[i];
      const v = o[k];
      h = h + hashMerge(getHash(v), hashString(k)) | 0;
    }
  }
  return h;
}
function getHash(u) {
  if (u === null)
    return 1108378658;
  if (u === undefined)
    return 1108378659;
  if (u === true)
    return 1108378657;
  if (u === false)
    return 1108378656;
  switch (typeof u) {
    case "number":
      return hashNumber(u);
    case "string":
      return hashString(u);
    case "bigint":
      return hashBigInt(u);
    case "object":
      return hashObject(u);
    case "symbol":
      return hashByReference(u);
    case "function":
      return hashByReference(u);
    default:
      return 0;
  }
}
var SHIFT = 5;
var BUCKET_SIZE = Math.pow(2, SHIFT);
var MASK = BUCKET_SIZE - 1;
var MAX_INDEX_NODE = BUCKET_SIZE / 2;
var MIN_ARRAY_NODE = BUCKET_SIZE / 4;
var ENTRY = 0;
var ARRAY_NODE = 1;
var INDEX_NODE = 2;
var COLLISION_NODE = 3;
var EMPTY = {
  type: INDEX_NODE,
  bitmap: 0,
  array: []
};
function mask(hash, shift) {
  return hash >>> shift & MASK;
}
function bitpos(hash, shift) {
  return 1 << mask(hash, shift);
}
function bitcount(x) {
  x -= x >> 1 & 1431655765;
  x = (x & 858993459) + (x >> 2 & 858993459);
  x = x + (x >> 4) & 252645135;
  x += x >> 8;
  x += x >> 16;
  return x & 127;
}
function index(bitmap, bit) {
  return bitcount(bitmap & bit - 1);
}
function cloneAndSet(arr, at, val) {
  const len = arr.length;
  const out = new Array(len);
  for (let i = 0;i < len; ++i) {
    out[i] = arr[i];
  }
  out[at] = val;
  return out;
}
function spliceIn(arr, at, val) {
  const len = arr.length;
  const out = new Array(len + 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  out[g++] = val;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function spliceOut(arr, at) {
  const len = arr.length;
  const out = new Array(len - 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  ++i;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function createNode(shift, key1, val1, key2hash, key2, val2) {
  const key1hash = getHash(key1);
  if (key1hash === key2hash) {
    return {
      type: COLLISION_NODE,
      hash: key1hash,
      array: [
        { type: ENTRY, k: key1, v: val1 },
        { type: ENTRY, k: key2, v: val2 }
      ]
    };
  }
  const addedLeaf = { val: false };
  return assoc(assocIndex(EMPTY, shift, key1hash, key1, val1, addedLeaf), shift, key2hash, key2, val2, addedLeaf);
}
function assoc(root, shift, hash, key, val, addedLeaf) {
  switch (root.type) {
    case ARRAY_NODE:
      return assocArray(root, shift, hash, key, val, addedLeaf);
    case INDEX_NODE:
      return assocIndex(root, shift, hash, key, val, addedLeaf);
    case COLLISION_NODE:
      return assocCollision(root, shift, hash, key, val, addedLeaf);
  }
}
function assocArray(root, shift, hash, key, val, addedLeaf) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === undefined) {
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root.size + 1,
      array: cloneAndSet(root.array, idx, { type: ENTRY, k: key, v: val })
    };
  }
  if (node.type === ENTRY) {
    if (isEqual(key, node.k)) {
      if (val === node.v) {
        return root;
      }
      return {
        type: ARRAY_NODE,
        size: root.size,
        array: cloneAndSet(root.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root.size,
      array: cloneAndSet(root.array, idx, createNode(shift + SHIFT, node.k, node.v, hash, key, val))
    };
  }
  const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
  if (n === node) {
    return root;
  }
  return {
    type: ARRAY_NODE,
    size: root.size,
    array: cloneAndSet(root.array, idx, n)
  };
}
function assocIndex(root, shift, hash, key, val, addedLeaf) {
  const bit = bitpos(hash, shift);
  const idx = index(root.bitmap, bit);
  if ((root.bitmap & bit) !== 0) {
    const node = root.array[idx];
    if (node.type !== ENTRY) {
      const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
      if (n === node) {
        return root;
      }
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, n)
      };
    }
    const nodeKey = node.k;
    if (isEqual(key, nodeKey)) {
      if (val === node.v) {
        return root;
      }
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap,
      array: cloneAndSet(root.array, idx, createNode(shift + SHIFT, nodeKey, node.v, hash, key, val))
    };
  } else {
    const n = root.array.length;
    if (n >= MAX_INDEX_NODE) {
      const nodes = new Array(32);
      const jdx = mask(hash, shift);
      nodes[jdx] = assocIndex(EMPTY, shift + SHIFT, hash, key, val, addedLeaf);
      let j = 0;
      let bitmap = root.bitmap;
      for (let i = 0;i < 32; i++) {
        if ((bitmap & 1) !== 0) {
          const node = root.array[j++];
          nodes[i] = node;
        }
        bitmap = bitmap >>> 1;
      }
      return {
        type: ARRAY_NODE,
        size: n + 1,
        array: nodes
      };
    } else {
      const newArray = spliceIn(root.array, idx, {
        type: ENTRY,
        k: key,
        v: val
      });
      addedLeaf.val = true;
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap | bit,
        array: newArray
      };
    }
  }
}
function assocCollision(root, shift, hash, key, val, addedLeaf) {
  if (hash === root.hash) {
    const idx = collisionIndexOf(root, key);
    if (idx !== -1) {
      const entry = root.array[idx];
      if (entry.v === val) {
        return root;
      }
      return {
        type: COLLISION_NODE,
        hash,
        array: cloneAndSet(root.array, idx, { type: ENTRY, k: key, v: val })
      };
    }
    const size = root.array.length;
    addedLeaf.val = true;
    return {
      type: COLLISION_NODE,
      hash,
      array: cloneAndSet(root.array, size, { type: ENTRY, k: key, v: val })
    };
  }
  return assoc({
    type: INDEX_NODE,
    bitmap: bitpos(root.hash, shift),
    array: [root]
  }, shift, hash, key, val, addedLeaf);
}
function collisionIndexOf(root, key) {
  const size = root.array.length;
  for (let i = 0;i < size; i++) {
    if (isEqual(key, root.array[i].k)) {
      return i;
    }
  }
  return -1;
}
function find(root, shift, hash, key) {
  switch (root.type) {
    case ARRAY_NODE:
      return findArray(root, shift, hash, key);
    case INDEX_NODE:
      return findIndex(root, shift, hash, key);
    case COLLISION_NODE:
      return findCollision(root, key);
  }
}
function findArray(root, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === undefined) {
    return;
  }
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return;
}
function findIndex(root, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root.bitmap & bit) === 0) {
    return;
  }
  const idx = index(root.bitmap, bit);
  const node = root.array[idx];
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return;
}
function findCollision(root, key) {
  const idx = collisionIndexOf(root, key);
  if (idx < 0) {
    return;
  }
  return root.array[idx];
}
function without(root, shift, hash, key) {
  switch (root.type) {
    case ARRAY_NODE:
      return withoutArray(root, shift, hash, key);
    case INDEX_NODE:
      return withoutIndex(root, shift, hash, key);
    case COLLISION_NODE:
      return withoutCollision(root, key);
  }
}
function withoutArray(root, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === undefined) {
    return root;
  }
  let n = undefined;
  if (node.type === ENTRY) {
    if (!isEqual(node.k, key)) {
      return root;
    }
  } else {
    n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root;
    }
  }
  if (n === undefined) {
    if (root.size <= MIN_ARRAY_NODE) {
      const arr = root.array;
      const out = new Array(root.size - 1);
      let i = 0;
      let j = 0;
      let bitmap = 0;
      while (i < idx) {
        const nv = arr[i];
        if (nv !== undefined) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      ++i;
      while (i < arr.length) {
        const nv = arr[i];
        if (nv !== undefined) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      return {
        type: INDEX_NODE,
        bitmap,
        array: out
      };
    }
    return {
      type: ARRAY_NODE,
      size: root.size - 1,
      array: cloneAndSet(root.array, idx, n)
    };
  }
  return {
    type: ARRAY_NODE,
    size: root.size,
    array: cloneAndSet(root.array, idx, n)
  };
}
function withoutIndex(root, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root.bitmap & bit) === 0) {
    return root;
  }
  const idx = index(root.bitmap, bit);
  const node = root.array[idx];
  if (node.type !== ENTRY) {
    const n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root;
    }
    if (n !== undefined) {
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, n)
      };
    }
    if (root.bitmap === bit) {
      return;
    }
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap ^ bit,
      array: spliceOut(root.array, idx)
    };
  }
  if (isEqual(key, node.k)) {
    if (root.bitmap === bit) {
      return;
    }
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap ^ bit,
      array: spliceOut(root.array, idx)
    };
  }
  return root;
}
function withoutCollision(root, key) {
  const idx = collisionIndexOf(root, key);
  if (idx < 0) {
    return root;
  }
  if (root.array.length === 1) {
    return;
  }
  return {
    type: COLLISION_NODE,
    hash: root.hash,
    array: spliceOut(root.array, idx)
  };
}
function forEach(root, fn) {
  if (root === undefined) {
    return;
  }
  const items = root.array;
  const size = items.length;
  for (let i = 0;i < size; i++) {
    const item = items[i];
    if (item === undefined) {
      continue;
    }
    if (item.type === ENTRY) {
      fn(item.v, item.k);
      continue;
    }
    forEach(item, fn);
  }
}

class Dict {
  static fromObject(o) {
    const keys = Object.keys(o);
    let m = Dict.new();
    for (let i = 0;i < keys.length; i++) {
      const k = keys[i];
      m = m.set(k, o[k]);
    }
    return m;
  }
  static fromMap(o) {
    let m = Dict.new();
    o.forEach((v, k) => {
      m = m.set(k, v);
    });
    return m;
  }
  static new() {
    return new Dict(undefined, 0);
  }
  constructor(root, size) {
    this.root = root;
    this.size = size;
  }
  get(key, notFound) {
    if (this.root === undefined) {
      return notFound;
    }
    const found = find(this.root, 0, getHash(key), key);
    if (found === undefined) {
      return notFound;
    }
    return found.v;
  }
  set(key, val) {
    const addedLeaf = { val: false };
    const root = this.root === undefined ? EMPTY : this.root;
    const newRoot = assoc(root, 0, getHash(key), key, val, addedLeaf);
    if (newRoot === this.root) {
      return this;
    }
    return new Dict(newRoot, addedLeaf.val ? this.size + 1 : this.size);
  }
  delete(key) {
    if (this.root === undefined) {
      return this;
    }
    const newRoot = without(this.root, 0, getHash(key), key);
    if (newRoot === this.root) {
      return this;
    }
    if (newRoot === undefined) {
      return Dict.new();
    }
    return new Dict(newRoot, this.size - 1);
  }
  has(key) {
    if (this.root === undefined) {
      return false;
    }
    return find(this.root, 0, getHash(key), key) !== undefined;
  }
  entries() {
    if (this.root === undefined) {
      return [];
    }
    const result = [];
    this.forEach((v, k) => result.push([k, v]));
    return result;
  }
  forEach(fn) {
    forEach(this.root, fn);
  }
  hashCode() {
    let h = 0;
    this.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
    return h;
  }
  equals(o) {
    if (!(o instanceof Dict) || this.size !== o.size) {
      return false;
    }
    try {
      this.forEach((v, k) => {
        if (!isEqual(o.get(k, !v), v)) {
          throw unequalDictSymbol;
        }
      });
      return true;
    } catch (e) {
      if (e === unequalDictSymbol) {
        return false;
      }
      throw e;
    }
  }
}
var unequalDictSymbol = /* @__PURE__ */ Symbol();

// build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs
var Nil = undefined;
var NOT_FOUND = {};
function identity(x) {
  return x;
}
function parse_int(value) {
  if (/^[-+]?(\d+)$/.test(value)) {
    return new Ok(parseInt(value));
  } else {
    return new Error(Nil);
  }
}
function to_string(term) {
  return term.toString();
}
function float_to_string(float) {
  const string = float.toString().replace("+", "");
  if (string.indexOf(".") >= 0) {
    return string;
  } else {
    const index2 = string.indexOf("e");
    if (index2 >= 0) {
      return string.slice(0, index2) + ".0" + string.slice(index2);
    } else {
      return string + ".0";
    }
  }
}
function string_replace(string, target, substitute) {
  if (typeof string.replaceAll !== "undefined") {
    return string.replaceAll(target, substitute);
  }
  return string.replace(new RegExp(target.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"), "g"), substitute);
}
function graphemes(string) {
  const iterator = graphemes_iterator(string);
  if (iterator) {
    return List.fromArray(Array.from(iterator).map((item) => item.segment));
  } else {
    return List.fromArray(string.match(/./gsu));
  }
}
var segmenter = undefined;
function graphemes_iterator(string) {
  if (globalThis.Intl && Intl.Segmenter) {
    segmenter ||= new Intl.Segmenter;
    return segmenter.segment(string)[Symbol.iterator]();
  }
}
function less_than(a, b) {
  return a < b;
}
function split(xs, pattern) {
  return List.fromArray(xs.split(pattern));
}
function join(xs, separator) {
  const iterator = xs[Symbol.iterator]();
  let result = iterator.next().value || "";
  let current = iterator.next();
  while (!current.done) {
    result = result + separator + current.value;
    current = iterator.next();
  }
  return result;
}
var unicode_whitespaces = [
  " ",
  "\t",
  `
`,
  "\v",
  "\f",
  "\r",
  "\x85",
  "\u2028",
  "\u2029"
].join("");
var trim_start_regex = /* @__PURE__ */ new RegExp(`^[${unicode_whitespaces}]*`);
var trim_end_regex = /* @__PURE__ */ new RegExp(`[${unicode_whitespaces}]*$`);
function trim_start(string) {
  return string.replace(trim_start_regex, "");
}
function trim_end(string) {
  return string.replace(trim_end_regex, "");
}
function bit_array_from_string(string) {
  return toBitArray([stringBits(string)]);
}
function console_error(term) {
  console.error(term);
}
function new_map() {
  return Dict.new();
}
function map_size(map) {
  return map.size;
}
function map_to_list(map) {
  return List.fromArray(map.entries());
}
function map_get(map, key) {
  const value = map.get(key, NOT_FOUND);
  if (value === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value);
}
function map_insert(key, value, map) {
  return map.set(key, value);
}
function classify_dynamic(data) {
  if (typeof data === "string") {
    return "String";
  } else if (typeof data === "boolean") {
    return "Bool";
  } else if (data instanceof Result) {
    return "Result";
  } else if (data instanceof List) {
    return "List";
  } else if (data instanceof BitArray) {
    return "BitArray";
  } else if (data instanceof Dict) {
    return "Dict";
  } else if (Number.isInteger(data)) {
    return "Int";
  } else if (Array.isArray(data)) {
    return `Tuple of ${data.length} elements`;
  } else if (typeof data === "number") {
    return "Float";
  } else if (data === null) {
    return "Null";
  } else if (data === undefined) {
    return "Nil";
  } else {
    const type = typeof data;
    return type.charAt(0).toUpperCase() + type.slice(1);
  }
}
function inspect(v) {
  const t = typeof v;
  if (v === true)
    return "True";
  if (v === false)
    return "False";
  if (v === null)
    return "//js(null)";
  if (v === undefined)
    return "Nil";
  if (t === "string")
    return inspectString(v);
  if (t === "bigint" || Number.isInteger(v))
    return v.toString();
  if (t === "number")
    return float_to_string(v);
  if (Array.isArray(v))
    return `#(${v.map(inspect).join(", ")})`;
  if (v instanceof List)
    return inspectList(v);
  if (v instanceof UtfCodepoint)
    return inspectUtfCodepoint(v);
  if (v instanceof BitArray)
    return `<<${bit_array_inspect(v, "")}>>`;
  if (v instanceof CustomType)
    return inspectCustomType(v);
  if (v instanceof Dict)
    return inspectDict(v);
  if (v instanceof Set)
    return `//js(Set(${[...v].map(inspect).join(", ")}))`;
  if (v instanceof RegExp)
    return `//js(${v})`;
  if (v instanceof Date)
    return `//js(Date("${v.toISOString()}"))`;
  if (v instanceof Function) {
    const args = [];
    for (const i of Array(v.length).keys())
      args.push(String.fromCharCode(i + 97));
    return `//fn(${args.join(", ")}) { ... }`;
  }
  return inspectObject(v);
}
function inspectString(str) {
  let new_str = '"';
  for (let i = 0;i < str.length; i++) {
    const char = str[i];
    switch (char) {
      case `
`:
        new_str += "\\n";
        break;
      case "\r":
        new_str += "\\r";
        break;
      case "\t":
        new_str += "\\t";
        break;
      case "\f":
        new_str += "\\f";
        break;
      case "\\":
        new_str += "\\\\";
        break;
      case '"':
        new_str += "\\\"";
        break;
      default:
        if (char < " " || char > "~" && char < "\xA0") {
          new_str += "\\u{" + char.charCodeAt(0).toString(16).toUpperCase().padStart(4, "0") + "}";
        } else {
          new_str += char;
        }
    }
  }
  new_str += '"';
  return new_str;
}
function inspectDict(map) {
  let body = "dict.from_list([";
  let first = true;
  map.forEach((value, key) => {
    if (!first)
      body = body + ", ";
    body = body + "#(" + inspect(key) + ", " + inspect(value) + ")";
    first = false;
  });
  return body + "])";
}
function inspectObject(v) {
  const name = Object.getPrototypeOf(v)?.constructor?.name || "Object";
  const props = [];
  for (const k of Object.keys(v)) {
    props.push(`${inspect(k)}: ${inspect(v[k])}`);
  }
  const body = props.length ? " " + props.join(", ") + " " : "";
  const head = name === "Object" ? "" : name + " ";
  return `//js(${head}{${body}})`;
}
function inspectCustomType(record) {
  const props = Object.keys(record).map((label) => {
    const value = inspect(record[label]);
    return isNaN(parseInt(label)) ? `${label}: ${value}` : value;
  }).join(", ");
  return props ? `${record.constructor.name}(${props})` : record.constructor.name;
}
function inspectList(list) {
  return `[${list.toArray().map(inspect).join(", ")}]`;
}
function inspectUtfCodepoint(codepoint) {
  return `//utfcodepoint(${String.fromCodePoint(codepoint.value)})`;
}
function bit_array_inspect(bits, acc) {
  if (bits.bitSize === 0) {
    return acc;
  }
  for (let i = 0;i < bits.byteSize - 1; i++) {
    acc += bits.byteAt(i).toString();
    acc += ", ";
  }
  if (bits.byteSize * 8 === bits.bitSize) {
    acc += bits.byteAt(bits.byteSize - 1).toString();
  } else {
    const trailingBitsCount = bits.bitSize % 8;
    acc += bits.byteAt(bits.byteSize - 1) >> 8 - trailingBitsCount;
    acc += `:size(${trailingBitsCount})`;
  }
  return acc;
}

// build/dev/javascript/gleam_stdlib/gleam/dict.mjs
function do_has_key(key, dict) {
  return !isEqual(map_get(dict, key), new Error(undefined));
}
function has_key(dict, key) {
  return do_has_key(key, dict);
}
function insert(dict, key, value) {
  return map_insert(key, value, dict);
}
function reverse_and_concat(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining instanceof Empty) {
      return accumulator;
    } else {
      let first = remaining.head;
      let rest = remaining.tail;
      loop$remaining = rest;
      loop$accumulator = prepend(first, accumulator);
    }
  }
}
function do_keys_loop(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list instanceof Empty) {
      return reverse_and_concat(acc, toList([]));
    } else {
      let rest = list.tail;
      let key = list.head[0];
      loop$list = rest;
      loop$acc = prepend(key, acc);
    }
  }
}
function keys(dict) {
  return do_keys_loop(map_to_list(dict), toList([]));
}
function upsert(dict, key, fun) {
  let $ = map_get(dict, key);
  if ($ instanceof Ok) {
    let value = $[0];
    return insert(dict, key, fun(new Some(value)));
  } else {
    return insert(dict, key, fun(new None));
  }
}
function fold_loop(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list instanceof Empty) {
      return initial;
    } else {
      let rest = list.tail;
      let k = list.head[0];
      let v = list.head[1];
      loop$list = rest;
      loop$initial = fun(initial, k, v);
      loop$fun = fun;
    }
  }
}
function fold(dict, initial, fun) {
  return fold_loop(map_to_list(dict), initial, fun);
}
function do_filter(f, dict) {
  let insert$1 = (dict2, k, v) => {
    let $ = f(k, v);
    if ($) {
      return insert(dict2, k, v);
    } else {
      return dict2;
    }
  };
  return fold(dict, new_map(), insert$1);
}
function filter(dict, predicate) {
  return do_filter(predicate, dict);
}

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
class Ascending extends CustomType {
}

class Descending extends CustomType {
}
function length_loop(loop$list, loop$count) {
  while (true) {
    let list = loop$list;
    let count = loop$count;
    if (list instanceof Empty) {
      return count;
    } else {
      let list$1 = list.tail;
      loop$list = list$1;
      loop$count = count + 1;
    }
  }
}
function length(list) {
  return length_loop(list, 0);
}
function reverse_and_prepend(loop$prefix, loop$suffix) {
  while (true) {
    let prefix = loop$prefix;
    let suffix = loop$suffix;
    if (prefix instanceof Empty) {
      return suffix;
    } else {
      let first$1 = prefix.head;
      let rest$1 = prefix.tail;
      loop$prefix = rest$1;
      loop$suffix = prepend(first$1, suffix);
    }
  }
}
function reverse(list) {
  return reverse_and_prepend(list, toList([]));
}
function is_empty(list) {
  return isEqual(list, toList([]));
}
function first(list) {
  if (list instanceof Empty) {
    return new Error(undefined);
  } else {
    let first$1 = list.head;
    return new Ok(first$1);
  }
}
function filter_loop(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list instanceof Empty) {
      return reverse(acc);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let _block;
      let $ = fun(first$1);
      if ($) {
        _block = prepend(first$1, acc);
      } else {
        _block = acc;
      }
      let new_acc = _block;
      loop$list = rest$1;
      loop$fun = fun;
      loop$acc = new_acc;
    }
  }
}
function filter2(list, predicate) {
  return filter_loop(list, predicate, toList([]));
}
function filter_map_loop(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list instanceof Empty) {
      return reverse(acc);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let _block;
      let $ = fun(first$1);
      if ($ instanceof Ok) {
        let first$2 = $[0];
        _block = prepend(first$2, acc);
      } else {
        _block = acc;
      }
      let new_acc = _block;
      loop$list = rest$1;
      loop$fun = fun;
      loop$acc = new_acc;
    }
  }
}
function filter_map(list, fun) {
  return filter_map_loop(list, fun, toList([]));
}
function map_loop(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list instanceof Empty) {
      return reverse(acc);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$fun = fun;
      loop$acc = prepend(fun(first$1), acc);
    }
  }
}
function map(list, fun) {
  return map_loop(list, fun, toList([]));
}
function index_map_loop(loop$list, loop$fun, loop$index, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let index2 = loop$index;
    let acc = loop$acc;
    if (list instanceof Empty) {
      return reverse(acc);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let acc$1 = prepend(fun(first$1, index2), acc);
      loop$list = rest$1;
      loop$fun = fun;
      loop$index = index2 + 1;
      loop$acc = acc$1;
    }
  }
}
function index_map(list, fun) {
  return index_map_loop(list, fun, 0, toList([]));
}
function drop(loop$list, loop$n) {
  while (true) {
    let list = loop$list;
    let n = loop$n;
    let $ = n <= 0;
    if ($) {
      return list;
    } else {
      if (list instanceof Empty) {
        return list;
      } else {
        let rest$1 = list.tail;
        loop$list = rest$1;
        loop$n = n - 1;
      }
    }
  }
}
function append_loop(loop$first, loop$second) {
  while (true) {
    let first2 = loop$first;
    let second = loop$second;
    if (first2 instanceof Empty) {
      return second;
    } else {
      let first$1 = first2.head;
      let rest$1 = first2.tail;
      loop$first = rest$1;
      loop$second = prepend(first$1, second);
    }
  }
}
function append(first2, second) {
  return append_loop(reverse(first2), second);
}
function flatten_loop(loop$lists, loop$acc) {
  while (true) {
    let lists = loop$lists;
    let acc = loop$acc;
    if (lists instanceof Empty) {
      return reverse(acc);
    } else {
      let list = lists.head;
      let further_lists = lists.tail;
      loop$lists = further_lists;
      loop$acc = reverse_and_prepend(list, acc);
    }
  }
}
function flatten(lists) {
  return flatten_loop(lists, toList([]));
}
function fold2(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list instanceof Empty) {
      return initial;
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$initial = fun(initial, first$1);
      loop$fun = fun;
    }
  }
}
function try_fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list instanceof Empty) {
      return new Ok(initial);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = fun(initial, first$1);
      if ($ instanceof Ok) {
        let result = $[0];
        loop$list = rest$1;
        loop$initial = result;
        loop$fun = fun;
      } else {
        return $;
      }
    }
  }
}
function sequences(loop$list, loop$compare, loop$growing, loop$direction, loop$prev, loop$acc) {
  while (true) {
    let list = loop$list;
    let compare3 = loop$compare;
    let growing = loop$growing;
    let direction = loop$direction;
    let prev = loop$prev;
    let acc = loop$acc;
    let growing$1 = prepend(prev, growing);
    if (list instanceof Empty) {
      if (direction instanceof Ascending) {
        return prepend(reverse(growing$1), acc);
      } else {
        return prepend(growing$1, acc);
      }
    } else {
      let new$1 = list.head;
      let rest$1 = list.tail;
      let $ = compare3(prev, new$1);
      if (direction instanceof Ascending) {
        if ($ instanceof Lt) {
          loop$list = rest$1;
          loop$compare = compare3;
          loop$growing = growing$1;
          loop$direction = direction;
          loop$prev = new$1;
          loop$acc = acc;
        } else if ($ instanceof Eq) {
          loop$list = rest$1;
          loop$compare = compare3;
          loop$growing = growing$1;
          loop$direction = direction;
          loop$prev = new$1;
          loop$acc = acc;
        } else {
          let _block;
          if (direction instanceof Ascending) {
            _block = prepend(reverse(growing$1), acc);
          } else {
            _block = prepend(growing$1, acc);
          }
          let acc$1 = _block;
          if (rest$1 instanceof Empty) {
            return prepend(toList([new$1]), acc$1);
          } else {
            let next = rest$1.head;
            let rest$2 = rest$1.tail;
            let _block$1;
            let $1 = compare3(new$1, next);
            if ($1 instanceof Lt) {
              _block$1 = new Ascending;
            } else if ($1 instanceof Eq) {
              _block$1 = new Ascending;
            } else {
              _block$1 = new Descending;
            }
            let direction$1 = _block$1;
            loop$list = rest$2;
            loop$compare = compare3;
            loop$growing = toList([new$1]);
            loop$direction = direction$1;
            loop$prev = next;
            loop$acc = acc$1;
          }
        }
      } else if ($ instanceof Lt) {
        let _block;
        if (direction instanceof Ascending) {
          _block = prepend(reverse(growing$1), acc);
        } else {
          _block = prepend(growing$1, acc);
        }
        let acc$1 = _block;
        if (rest$1 instanceof Empty) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let _block$1;
          let $1 = compare3(new$1, next);
          if ($1 instanceof Lt) {
            _block$1 = new Ascending;
          } else if ($1 instanceof Eq) {
            _block$1 = new Ascending;
          } else {
            _block$1 = new Descending;
          }
          let direction$1 = _block$1;
          loop$list = rest$2;
          loop$compare = compare3;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else if ($ instanceof Eq) {
        let _block;
        if (direction instanceof Ascending) {
          _block = prepend(reverse(growing$1), acc);
        } else {
          _block = prepend(growing$1, acc);
        }
        let acc$1 = _block;
        if (rest$1 instanceof Empty) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let _block$1;
          let $1 = compare3(new$1, next);
          if ($1 instanceof Lt) {
            _block$1 = new Ascending;
          } else if ($1 instanceof Eq) {
            _block$1 = new Ascending;
          } else {
            _block$1 = new Descending;
          }
          let direction$1 = _block$1;
          loop$list = rest$2;
          loop$compare = compare3;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else {
        loop$list = rest$1;
        loop$compare = compare3;
        loop$growing = growing$1;
        loop$direction = direction;
        loop$prev = new$1;
        loop$acc = acc;
      }
    }
  }
}
function merge_ascendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list2 = loop$list2;
    let compare3 = loop$compare;
    let acc = loop$acc;
    if (list1 instanceof Empty) {
      let list = list2;
      return reverse_and_prepend(list, acc);
    } else if (list2 instanceof Empty) {
      let list = list1;
      return reverse_and_prepend(list, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list2.head;
      let rest2 = list2.tail;
      let $ = compare3(first1, first2);
      if ($ instanceof Lt) {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare3;
        loop$acc = prepend(first1, acc);
      } else if ($ instanceof Eq) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare3;
        loop$acc = prepend(first2, acc);
      } else {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare3;
        loop$acc = prepend(first2, acc);
      }
    }
  }
}
function merge_ascending_pairs(loop$sequences, loop$compare, loop$acc) {
  while (true) {
    let sequences2 = loop$sequences;
    let compare3 = loop$compare;
    let acc = loop$acc;
    if (sequences2 instanceof Empty) {
      return reverse(acc);
    } else {
      let $ = sequences2.tail;
      if ($ instanceof Empty) {
        let sequence = sequences2.head;
        return reverse(prepend(reverse(sequence), acc));
      } else {
        let ascending1 = sequences2.head;
        let ascending2 = $.head;
        let rest$1 = $.tail;
        let descending = merge_ascendings(ascending1, ascending2, compare3, toList([]));
        loop$sequences = rest$1;
        loop$compare = compare3;
        loop$acc = prepend(descending, acc);
      }
    }
  }
}
function merge_descendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list2 = loop$list2;
    let compare3 = loop$compare;
    let acc = loop$acc;
    if (list1 instanceof Empty) {
      let list = list2;
      return reverse_and_prepend(list, acc);
    } else if (list2 instanceof Empty) {
      let list = list1;
      return reverse_and_prepend(list, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list2.head;
      let rest2 = list2.tail;
      let $ = compare3(first1, first2);
      if ($ instanceof Lt) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare3;
        loop$acc = prepend(first2, acc);
      } else if ($ instanceof Eq) {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare3;
        loop$acc = prepend(first1, acc);
      } else {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare3;
        loop$acc = prepend(first1, acc);
      }
    }
  }
}
function merge_descending_pairs(loop$sequences, loop$compare, loop$acc) {
  while (true) {
    let sequences2 = loop$sequences;
    let compare3 = loop$compare;
    let acc = loop$acc;
    if (sequences2 instanceof Empty) {
      return reverse(acc);
    } else {
      let $ = sequences2.tail;
      if ($ instanceof Empty) {
        let sequence = sequences2.head;
        return reverse(prepend(reverse(sequence), acc));
      } else {
        let descending1 = sequences2.head;
        let descending2 = $.head;
        let rest$1 = $.tail;
        let ascending = merge_descendings(descending1, descending2, compare3, toList([]));
        loop$sequences = rest$1;
        loop$compare = compare3;
        loop$acc = prepend(ascending, acc);
      }
    }
  }
}
function merge_all(loop$sequences, loop$direction, loop$compare) {
  while (true) {
    let sequences2 = loop$sequences;
    let direction = loop$direction;
    let compare3 = loop$compare;
    if (sequences2 instanceof Empty) {
      return sequences2;
    } else if (direction instanceof Ascending) {
      let $ = sequences2.tail;
      if ($ instanceof Empty) {
        let sequence = sequences2.head;
        return sequence;
      } else {
        let sequences$1 = merge_ascending_pairs(sequences2, compare3, toList([]));
        loop$sequences = sequences$1;
        loop$direction = new Descending;
        loop$compare = compare3;
      }
    } else {
      let $ = sequences2.tail;
      if ($ instanceof Empty) {
        let sequence = sequences2.head;
        return reverse(sequence);
      } else {
        let sequences$1 = merge_descending_pairs(sequences2, compare3, toList([]));
        loop$sequences = sequences$1;
        loop$direction = new Ascending;
        loop$compare = compare3;
      }
    }
  }
}
function sort(list, compare3) {
  if (list instanceof Empty) {
    return list;
  } else {
    let $ = list.tail;
    if ($ instanceof Empty) {
      return list;
    } else {
      let x = list.head;
      let y = $.head;
      let rest$1 = $.tail;
      let _block;
      let $1 = compare3(x, y);
      if ($1 instanceof Lt) {
        _block = new Ascending;
      } else if ($1 instanceof Eq) {
        _block = new Ascending;
      } else {
        _block = new Descending;
      }
      let direction = _block;
      let sequences$1 = sequences(rest$1, compare3, toList([x]), direction, y, toList([]));
      return merge_all(sequences$1, new Ascending, compare3);
    }
  }
}
function each(loop$list, loop$f) {
  while (true) {
    let list = loop$list;
    let f = loop$f;
    if (list instanceof Empty) {
      return;
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      f(first$1);
      loop$list = rest$1;
      loop$f = f;
    }
  }
}

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
function replace(string, pattern, substitute) {
  let _pipe = string;
  let _pipe$1 = identity(_pipe);
  let _pipe$2 = string_replace(_pipe$1, pattern, substitute);
  return identity(_pipe$2);
}
function compare3(a, b) {
  let $ = a === b;
  if ($) {
    return new Eq;
  } else {
    let $1 = less_than(a, b);
    if ($1) {
      return new Lt;
    } else {
      return new Gt;
    }
  }
}
function trim(string) {
  let _pipe = string;
  let _pipe$1 = trim_start(_pipe);
  return trim_end(_pipe$1);
}
function split2(x, substring) {
  if (substring === "") {
    return graphemes(x);
  } else {
    let _pipe = x;
    let _pipe$1 = identity(_pipe);
    let _pipe$2 = split(_pipe$1, substring);
    return map(_pipe$2, identity);
  }
}
function inspect2(term) {
  let _pipe = inspect(term);
  return identity(_pipe);
}

// build/dev/javascript/gleam_stdlib/gleam/result.mjs
function is_ok(result) {
  if (result instanceof Ok) {
    return true;
  } else {
    return false;
  }
}
function map_error(result, fun) {
  if (result instanceof Ok) {
    return result;
  } else {
    let error = result[0];
    return new Error(fun(error));
  }
}
function try$(result, fun) {
  if (result instanceof Ok) {
    let x = result[0];
    return fun(x);
  } else {
    return result;
  }
}
function then$(result, fun) {
  return try$(result, fun);
}
function replace_error(result, error) {
  if (result instanceof Ok) {
    return result;
  } else {
    return new Error(error);
  }
}
// build/dev/javascript/gleam_javascript/gleam_javascript_ffi.mjs
function toArray(list) {
  return list.toArray();
}
function reduceRight(thing, acc, fn) {
  return thing.reduceRight(fn, acc);
}
class PromiseLayer {
  constructor(promise) {
    this.promise = promise;
  }
  static wrap(value) {
    return value instanceof Promise ? new PromiseLayer(value) : value;
  }
  static unwrap(value) {
    return value instanceof PromiseLayer ? value.promise : value;
  }
}
function resolve(value) {
  return Promise.resolve(PromiseLayer.wrap(value));
}
function then_await(promise, fn) {
  return promise.then((value) => fn(PromiseLayer.unwrap(value)));
}
function map_promise(promise, fn) {
  return promise.then((value) => PromiseLayer.wrap(fn(PromiseLayer.unwrap(value))));
}

// build/dev/javascript/gleam_javascript/gleam/javascript/array.mjs
function to_list(items) {
  return reduceRight(items, toList([]), (list, item) => {
    return prepend(item, list);
  });
}

// build/dev/javascript/gleam_stdlib/gleam_stdlib_decode_ffi.mjs
function index3(data, key) {
  if (data instanceof Dict || data instanceof WeakMap || data instanceof Map) {
    const token = {};
    const entry = data.get(key, token);
    if (entry === token)
      return new Ok(new None);
    return new Ok(new Some(entry));
  }
  const key_is_int = Number.isInteger(key);
  if (key_is_int && key >= 0 && key < 8 && data instanceof List) {
    let i = 0;
    for (const value of data) {
      if (i === key)
        return new Ok(new Some(value));
      i++;
    }
    return new Error("Indexable");
  }
  if (key_is_int && Array.isArray(data) || data && typeof data === "object" || data && Object.getPrototypeOf(data) === Object.prototype) {
    if (key in data)
      return new Ok(new Some(data[key]));
    return new Ok(new None);
  }
  return new Error(key_is_int ? "Indexable" : "Dict");
}
function list(data, decode, pushPath, index4, emptyList) {
  if (!(data instanceof List || Array.isArray(data))) {
    const error = new DecodeError2("List", classify_dynamic(data), emptyList);
    return [emptyList, List.fromArray([error])];
  }
  const decoded = [];
  for (const element of data) {
    const layer = decode(element);
    const [out, errors] = layer;
    if (errors instanceof NonEmpty) {
      const [_, errors2] = pushPath(layer, index4.toString());
      return [emptyList, errors2];
    }
    decoded.push(out);
    index4++;
  }
  return [List.fromArray(decoded), emptyList];
}
function int(data) {
  if (Number.isInteger(data))
    return new Ok(data);
  return new Error(0);
}
function string(data) {
  if (typeof data === "string")
    return new Ok(data);
  return new Error(0);
}
function is_null(data) {
  return data === null || data === undefined;
}

// build/dev/javascript/gleam_stdlib/gleam/dynamic/decode.mjs
class DecodeError2 extends CustomType {
  constructor(expected, found, path) {
    super();
    this.expected = expected;
    this.found = found;
    this.path = path;
  }
}
class Decoder extends CustomType {
  constructor(function$) {
    super();
    this.function = function$;
  }
}
var dynamic = /* @__PURE__ */ new Decoder(decode_dynamic);
var bool = /* @__PURE__ */ new Decoder(decode_bool2);
var int2 = /* @__PURE__ */ new Decoder(decode_int2);
var string2 = /* @__PURE__ */ new Decoder(decode_string2);
function run(data, decoder) {
  let $ = decoder.function(data);
  let maybe_invalid_data;
  let errors;
  maybe_invalid_data = $[0];
  errors = $[1];
  if (errors instanceof Empty) {
    return new Ok(maybe_invalid_data);
  } else {
    return new Error(errors);
  }
}
function success(data) {
  return new Decoder((_) => {
    return [data, toList([])];
  });
}
function decode_dynamic(data) {
  return [data, toList([])];
}
function map4(decoder, transformer) {
  return new Decoder((d) => {
    let $ = decoder.function(d);
    let data;
    let errors;
    data = $[0];
    errors = $[1];
    return [transformer(data), errors];
  });
}
function run_decoders(loop$data, loop$failure, loop$decoders) {
  while (true) {
    let data = loop$data;
    let failure = loop$failure;
    let decoders = loop$decoders;
    if (decoders instanceof Empty) {
      return failure;
    } else {
      let decoder = decoders.head;
      let decoders$1 = decoders.tail;
      let $ = decoder.function(data);
      let layer;
      let errors;
      layer = $;
      errors = $[1];
      if (errors instanceof Empty) {
        return layer;
      } else {
        loop$data = data;
        loop$failure = failure;
        loop$decoders = decoders$1;
      }
    }
  }
}
function one_of(first2, alternatives) {
  return new Decoder((dynamic_data) => {
    let $ = first2.function(dynamic_data);
    let layer;
    let errors;
    layer = $;
    errors = $[1];
    if (errors instanceof Empty) {
      return layer;
    } else {
      return run_decoders(dynamic_data, layer, alternatives);
    }
  });
}
function optional(inner) {
  return new Decoder((data) => {
    let $ = is_null(data);
    if ($) {
      return [new None, toList([])];
    } else {
      let $1 = inner.function(data);
      let data$1;
      let errors;
      data$1 = $1[0];
      errors = $1[1];
      return [new Some(data$1), errors];
    }
  });
}
function decode_error(expected, found) {
  return toList([
    new DecodeError2(expected, classify_dynamic(found), toList([]))
  ]);
}
function run_dynamic_function(data, name, f) {
  let $ = f(data);
  if ($ instanceof Ok) {
    let data$1 = $[0];
    return [data$1, toList([])];
  } else {
    let zero = $[0];
    return [
      zero,
      toList([new DecodeError2(name, classify_dynamic(data), toList([]))])
    ];
  }
}
function decode_bool2(data) {
  let $ = isEqual(identity(true), data);
  if ($) {
    return [true, toList([])];
  } else {
    let $1 = isEqual(identity(false), data);
    if ($1) {
      return [false, toList([])];
    } else {
      return [false, decode_error("Bool", data)];
    }
  }
}
function decode_int2(data) {
  return run_dynamic_function(data, "Int", int);
}
function failure(zero, expected) {
  return new Decoder((d) => {
    return [zero, decode_error(expected, d)];
  });
}
function decode_string2(data) {
  return run_dynamic_function(data, "String", string);
}
function list2(inner) {
  return new Decoder((data) => {
    return list(data, inner.function, (p, k) => {
      return push_path(p, toList([k]));
    }, 0, toList([]));
  });
}
function push_path(layer, path) {
  let decoder = one_of(string2, toList([
    (() => {
      let _pipe = int2;
      return map4(_pipe, to_string);
    })()
  ]));
  let path$1 = map(path, (key) => {
    let key$1 = identity(key);
    let $ = run(key$1, decoder);
    if ($ instanceof Ok) {
      let key$2 = $[0];
      return key$2;
    } else {
      return "<" + classify_dynamic(key$1) + ">";
    }
  });
  let errors = map(layer[1], (error) => {
    return new DecodeError2(error.expected, error.found, append(path$1, error.path));
  });
  return [layer[0], errors];
}
function index4(loop$path, loop$position, loop$inner, loop$data, loop$handle_miss) {
  while (true) {
    let path = loop$path;
    let position = loop$position;
    let inner = loop$inner;
    let data = loop$data;
    let handle_miss = loop$handle_miss;
    if (path instanceof Empty) {
      let _pipe = inner(data);
      return push_path(_pipe, reverse(position));
    } else {
      let key = path.head;
      let path$1 = path.tail;
      let $ = index3(data, key);
      if ($ instanceof Ok) {
        let $1 = $[0];
        if ($1 instanceof Some) {
          let data$1 = $1[0];
          loop$path = path$1;
          loop$position = prepend(key, position);
          loop$inner = inner;
          loop$data = data$1;
          loop$handle_miss = handle_miss;
        } else {
          return handle_miss(data, prepend(key, position));
        }
      } else {
        let kind = $[0];
        let $1 = inner(data);
        let default$;
        default$ = $1[0];
        let _pipe = [
          default$,
          toList([new DecodeError2(kind, classify_dynamic(data), toList([]))])
        ];
        return push_path(_pipe, reverse(position));
      }
    }
  }
}
function subfield(field_path, field_decoder, next) {
  return new Decoder((data) => {
    let $ = index4(field_path, toList([]), field_decoder.function, data, (data2, position) => {
      let $12 = field_decoder.function(data2);
      let default$;
      default$ = $12[0];
      let _pipe = [
        default$,
        toList([new DecodeError2("Field", "Nothing", toList([]))])
      ];
      return push_path(_pipe, reverse(position));
    });
    let out;
    let errors1;
    out = $[0];
    errors1 = $[1];
    let $1 = next(out).function(data);
    let out$1;
    let errors2;
    out$1 = $1[0];
    errors2 = $1[1];
    return [out$1, append(errors1, errors2)];
  });
}
function field(field_name, field_decoder, next) {
  return subfield(toList([field_name]), field_decoder, next);
}
function optional_field(key, default$, field_decoder, next) {
  return new Decoder((data) => {
    let _block;
    let $1 = index3(data, key);
    if ($1 instanceof Ok) {
      let $22 = $1[0];
      if ($22 instanceof Some) {
        let data$1 = $22[0];
        _block = field_decoder.function(data$1);
      } else {
        _block = [default$, toList([])];
      }
    } else {
      let kind = $1[0];
      let _pipe = [
        default$,
        toList([new DecodeError2(kind, classify_dynamic(data), toList([]))])
      ];
      _block = push_path(_pipe, toList([key]));
    }
    let $ = _block;
    let out;
    let errors1;
    out = $[0];
    errors1 = $[1];
    let $2 = next(out).function(data);
    let out$1;
    let errors2;
    out$1 = $2[0];
    errors2 = $2[1];
    return [out$1, append(errors1, errors2)];
  });
}
// build/dev/javascript/gleam_json/gleam_json_ffi.mjs
function json_to_string(json) {
  return JSON.stringify(json);
}
function identity2(x) {
  return x;
}
function decode(string3) {
  try {
    const result = JSON.parse(string3);
    return new Ok(result);
  } catch (err) {
    return new Error(getJsonDecodeError(err, string3));
  }
}
function getJsonDecodeError(stdErr, json) {
  if (isUnexpectedEndOfInput(stdErr))
    return new UnexpectedEndOfInput;
  return toUnexpectedByteError(stdErr, json);
}
function isUnexpectedEndOfInput(err) {
  const unexpectedEndOfInputRegex = /((unexpected (end|eof))|(end of data)|(unterminated string)|(json( parse error|\.parse)\: expected '(\:|\}|\])'))/i;
  return unexpectedEndOfInputRegex.test(err.message);
}
function toUnexpectedByteError(err, json) {
  let converters = [
    v8UnexpectedByteError,
    oldV8UnexpectedByteError,
    jsCoreUnexpectedByteError,
    spidermonkeyUnexpectedByteError
  ];
  for (let converter of converters) {
    let result = converter(err, json);
    if (result)
      return result;
  }
  return new UnexpectedByte("", 0);
}
function v8UnexpectedByteError(err) {
  const regex = /unexpected token '(.)', ".+" is not valid JSON/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const byte = toHex(match[1]);
  return new UnexpectedByte(byte, -1);
}
function oldV8UnexpectedByteError(err) {
  const regex = /unexpected token (.) in JSON at position (\d+)/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const byte = toHex(match[1]);
  const position = Number(match[2]);
  return new UnexpectedByte(byte, position);
}
function spidermonkeyUnexpectedByteError(err, json) {
  const regex = /(unexpected character|expected .*) at line (\d+) column (\d+)/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const line = Number(match[2]);
  const column = Number(match[3]);
  const position = getPositionFromMultiline(line, column, json);
  const byte = toHex(json[position]);
  return new UnexpectedByte(byte, position);
}
function jsCoreUnexpectedByteError(err) {
  const regex = /unexpected (identifier|token) "(.)"/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const byte = toHex(match[2]);
  return new UnexpectedByte(byte, 0);
}
function toHex(char) {
  return "0x" + char.charCodeAt(0).toString(16).toUpperCase();
}
function getPositionFromMultiline(line, column, string3) {
  if (line === 1)
    return column - 1;
  let currentLn = 1;
  let position = 0;
  string3.split("").find((char, idx) => {
    if (char === `
`)
      currentLn += 1;
    if (currentLn === line) {
      position = idx + column;
      return true;
    }
    return false;
  });
  return position;
}

// build/dev/javascript/gleam_json/gleam/json.mjs
class UnexpectedEndOfInput extends CustomType {
}
class UnexpectedByte extends CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
class UnableToDecode extends CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
function do_parse(json, decoder) {
  return then$(decode(json), (dynamic_value) => {
    let _pipe = run(dynamic_value, decoder);
    return map_error(_pipe, (var0) => {
      return new UnableToDecode(var0);
    });
  });
}
function parse(json, decoder) {
  return do_parse(json, decoder);
}
function to_string2(json) {
  return json_to_string(json);
}
function string3(input) {
  return identity2(input);
}

// build/dev/javascript/gleam_stdlib/gleam/set.mjs
class Set2 extends CustomType {
  constructor(dict2) {
    super();
    this.dict = dict2;
  }
}
var token = undefined;
function new$() {
  return new Set2(new_map());
}
function contains(set, member) {
  let _pipe = set.dict;
  let _pipe$1 = map_get(_pipe, member);
  return is_ok(_pipe$1);
}
function to_list2(set) {
  return keys(set.dict);
}
function fold3(set, initial, reducer) {
  return fold(set.dict, initial, (a, k, _) => {
    return reducer(a, k);
  });
}
function insert2(set, member) {
  return new Set2(insert(set.dict, member, token));
}
function from_list2(members) {
  let dict2 = fold2(members, new_map(), (m, k) => {
    return insert(m, k, token);
  });
  return new Set2(dict2);
}
// build/dev/javascript/gleam_time/gleam/time/duration.mjs
class Duration extends CustomType {
  constructor(seconds, nanoseconds) {
    super();
    this.seconds = seconds;
    this.nanoseconds = nanoseconds;
  }
}
function normalise(duration) {
  let multiplier = 1e9;
  let nanoseconds$1 = remainderInt(duration.nanoseconds, multiplier);
  let overflow = duration.nanoseconds - nanoseconds$1;
  let seconds$1 = duration.seconds + divideInt(overflow, multiplier);
  let $ = nanoseconds$1 >= 0;
  if ($) {
    return new Duration(seconds$1, nanoseconds$1);
  } else {
    return new Duration(seconds$1 - 1, multiplier + nanoseconds$1);
  }
}
function add2(left, right) {
  let _pipe = new Duration(left.seconds + right.seconds, left.nanoseconds + right.nanoseconds);
  return normalise(_pipe);
}
function seconds(amount) {
  return new Duration(amount, 0);
}
function nanoseconds(amount) {
  let _pipe = new Duration(0, amount);
  return normalise(_pipe);
}
function to_seconds(duration) {
  let seconds$1 = identity(duration.seconds);
  let nanoseconds$1 = identity(duration.nanoseconds);
  return seconds$1 + nanoseconds$1 / 1e9;
}

// build/dev/javascript/gleam_time/gleam_time_ffi.mjs
function system_time() {
  const now = Date.now();
  const milliseconds = now % 1000;
  const nanoseconds2 = milliseconds * 1e6;
  const seconds2 = (now - milliseconds) / 1000;
  return [seconds2, nanoseconds2];
}

// build/dev/javascript/gleam_time/gleam/time/timestamp.mjs
class Timestamp extends CustomType {
  constructor(seconds2, nanoseconds2) {
    super();
    this.seconds = seconds2;
    this.nanoseconds = nanoseconds2;
  }
}
function normalise2(timestamp) {
  let multiplier = 1e9;
  let nanoseconds2 = remainderInt(timestamp.nanoseconds, multiplier);
  let overflow = timestamp.nanoseconds - nanoseconds2;
  let seconds2 = timestamp.seconds + divideInt(overflow, multiplier);
  let $ = nanoseconds2 >= 0;
  if ($) {
    return new Timestamp(seconds2, nanoseconds2);
  } else {
    return new Timestamp(seconds2 - 1, multiplier + nanoseconds2);
  }
}
function system_time2() {
  let $ = system_time();
  let seconds2;
  let nanoseconds2;
  seconds2 = $[0];
  nanoseconds2 = $[1];
  return normalise2(new Timestamp(seconds2, nanoseconds2));
}
function difference(left, right) {
  let seconds2 = seconds(right.seconds - left.seconds);
  let nanoseconds2 = nanoseconds(right.nanoseconds - left.nanoseconds);
  return add2(seconds2, nanoseconds2);
}
// build/dev/javascript/emacs_backbone/package_tracker_ffi.mjs
var packageTracker = {
  total: 0,
  installed: [],
  pending: [],
  deps: {},
  allInstalled: false
};
function initializePackageTracker(packages, depsEntries) {
  const deps = {};
  if (depsEntries) {
    for (const entry of depsEntries) {
      deps[entry[0]] = entry[1] ? [...entry[1]] : [];
    }
  }
  packageTracker = {
    total: packages.length,
    installed: [],
    pending: [...packages],
    deps,
    allInstalled: packages.length === 0
  };
  return packageTracker.allInstalled;
}
function updatePackageTracker(packageName) {
  const index5 = packageTracker.pending.indexOf(packageName);
  if (index5 !== -1) {
    packageTracker.pending.splice(index5, 1);
    packageTracker.installed.push(packageName);
    packageTracker.allInstalled = packageTracker.pending.length === 0;
  }
  return { ...packageTracker };
}
function getPackageTrackerStatus() {
  return { ...packageTracker };
}
function getFailedPackages() {
  const failed = new Set(packageTracker.pending);
  const reverseDeps = {};
  for (const [pkg, deps] of Object.entries(packageTracker.deps)) {
    for (const d of deps) {
      if (!reverseDeps[d])
        reverseDeps[d] = [];
      reverseDeps[d].push(pkg);
    }
  }
  const queue = [...failed];
  while (queue.length) {
    const current = queue.shift();
    const dependents = reverseDeps[current] || [];
    for (const dep of dependents) {
      if (!failed.has(dep)) {
        failed.add(dep);
        queue.push(dep);
      }
    }
  }
  return [...failed];
}

// build/dev/javascript/emacs_backbone/jsonrpc_ffi.mjs
var DEBUG = process.env.BACKBONE_DEBUG === "true";
var requestId = 0;
var pendingRequests = new Map;
var messageHandler = null;
var inputBuffer = Buffer.alloc(0);
function writeMessage(obj) {
  const json = JSON.stringify(obj);
  const byteLength = Buffer.byteLength(json, "utf-8");
  const header = `Content-Length: ${byteLength}\r
\r
`;
  process.stdout.write(header + json);
}
function parseMessages(data) {
  const chunk = typeof data === "string" ? Buffer.from(data, "utf-8") : data;
  inputBuffer = Buffer.concat([inputBuffer, chunk]);
  while (true) {
    const headerEndStr = `\r
\r
`;
    const headerEnd = inputBuffer.indexOf(headerEndStr);
    if (headerEnd === -1)
      break;
    const header = inputBuffer.slice(0, headerEnd).toString("utf-8");
    const match = header.match(/Content-Length:\s*(\d+)/i);
    if (!match) {
      const skipped = inputBuffer.slice(0, headerEnd + 4).toString("utf-8");
      if (DEBUG) {
        console.error("[jsonrpc] Skipping malformed header:", JSON.stringify(skipped));
      }
      inputBuffer = inputBuffer.slice(headerEnd + 4);
      continue;
    }
    const contentLength = parseInt(match[1], 10);
    const bodyStart = headerEnd + 4;
    if (inputBuffer.length < bodyStart + contentLength) {
      break;
    }
    const body = inputBuffer.slice(bodyStart, bodyStart + contentLength).toString("utf-8");
    inputBuffer = inputBuffer.slice(bodyStart + contentLength);
    try {
      const msg = JSON.parse(body);
      handleIncomingMessage(msg);
    } catch (e) {
      console.error("[jsonrpc] JSON parse error:", e.message, "body:", body.slice(0, 100));
      if (messageHandler) {
        messageHandler(new ParseErrorEvent(body));
      }
    }
  }
}
function handleIncomingMessage(msg) {
  if (DEBUG) {
    const now = new Date;
    console.error(`[${now.getHours()}:${now.getMinutes()}:${now.getSeconds()}.${now.getMilliseconds()}]`, "recv:", JSON.stringify(msg));
  }
  if (!msg || typeof msg !== "object") {
    console.error("[jsonrpc] Ignoring non-object message:", typeof msg, msg);
    return;
  }
  if (msg.id !== undefined && msg.id !== null && !msg.method) {
    const pending = pendingRequests.get(msg.id);
    if (pending) {
      clearTimeout(pending.timeout);
      pendingRequests.delete(msg.id);
      if (msg.error) {
        pending.resolve({
          ok: false,
          error: msg.error.message || JSON.stringify(msg.error)
        });
      } else {
        pending.resolve({
          ok: true,
          value: typeof msg.result === "string" ? msg.result : JSON.stringify(msg.result)
        });
      }
    }
    return;
  }
  if (!messageHandler)
    return;
  trackPackageInstallation(msg);
  if (msg.id !== undefined && msg.id !== null && msg.method) {
    messageHandler(new RequestEvent(msg.id, msg.method, msg.params || {}));
    return;
  }
  if (msg.method) {
    messageHandler(new NotificationEvent(msg.method, msg.params || {}));
    return;
  }
  if (msg.jsonrpc && Object.keys(msg).length <= 2) {
    console.error("[jsonrpc] Ignoring minimal jsonrpc message:", JSON.stringify(msg));
    return;
  }
  console.error("[jsonrpc] Unrecognized message format:", JSON.stringify(msg));
  messageHandler(new HandleErrorEvent("unknown", "Unrecognized message format"));
}
function trackPackageInstallation(msg) {
  if (msg.method !== "package_installed" || !msg.params)
    return;
  const packageName = msg.params.name;
  if (!packageName)
    return;
  const packageTracker2 = updatePackageTracker(packageName);
  console.error(`[${packageTracker2.installed.length}/${packageTracker2.total}] completed: ${packageName}`);
  if (DEBUG) {
    const pendingList = packageTracker2.pending.length > 0 ? packageTracker2.pending.slice(0, 5).join(", ") + (packageTracker2.pending.length > 5 ? `, ... (+${packageTracker2.pending.length - 5} more)` : "") : "none";
    console.error(`  ${packageTracker2.pending.length} packages still pending: ${pendingList}`);
  }
}
function setupStdioServer(handler) {
  messageHandler = handler;
  process.stdin.on("data", (chunk) => {
    parseMessages(chunk);
  });
  process.stdin.on("end", () => {
    console.error("stdin closed, shutting down");
    process.exit(0);
  });
  process.stdin.on("error", (e) => {
    console.error("stdin read error:", e.message);
    process.exit(1);
  });
  process.stdin.resume();
}
function sendNotification(method, params) {
  try {
    const msg = {
      jsonrpc: "2.0",
      method,
      params
    };
    if (DEBUG)
      console.error(`[notify] ${method}`, params);
    writeMessage(msg);
    return Promise.resolve({ ok: true, value: "" });
  } catch (error) {
    return Promise.resolve({
      ok: false,
      error: `Failed to send notification: ${error.message}`
    });
  }
}
function sendRequest(method, params, timeoutMs) {
  const id = ++requestId;
  return new Promise((resolve2) => {
    const timeout = setTimeout(() => {
      pendingRequests.delete(id);
      resolve2({
        ok: false,
        error: `Timeout waiting for response to ${method}`
      });
    }, timeoutMs);
    pendingRequests.set(id, { resolve: resolve2, timeout });
    const msg = {
      jsonrpc: "2.0",
      id,
      method,
      params
    };
    if (DEBUG)
      console.error(`[request] id=${id} ${method}`, params);
    writeMessage(msg);
  });
}
function showMessage(message) {
  return sendNotification("show-message", { content: message });
}
function evalCode(code) {
  return sendNotification("eval-code", { content: code });
}
function fetchVar(expr, timeoutMs) {
  return sendRequest("fetch-var", { expr }, timeoutMs);
}
function sendResponse(id, result) {
  const msg = {
    jsonrpc: "2.0",
    id,
    result
  };
  if (DEBUG)
    console.error(`[response] id=${id}`, result);
  writeMessage(msg);
}
function sendErrorResponse(id, code, message) {
  const msg = {
    jsonrpc: "2.0",
    id,
    error: { code, message }
  };
  if (DEBUG)
    console.error(`[error-response] id=${id}`, code, message);
  writeMessage(msg);
}
function enableDebug(flag) {
  DEBUG = Boolean(flag);
}
var INIT_START_TIME = null;
function updateInitStartTime(startTime) {
  INIT_START_TIME = startTime;
}
function getInitStartTime() {
  return INIT_START_TIME;
}
function shutdown() {
  setImmediate(() => {
    process.exit(0);
  });
}

// build/dev/javascript/emacs_backbone/jsonrpc.mjs
class RequestEvent extends CustomType {
  constructor(id, method, params) {
    super();
    this.id = id;
    this.method = method;
    this.params = params;
  }
}
class NotificationEvent extends CustomType {
  constructor(method, params) {
    super();
    this.method = method;
    this.params = params;
  }
}
class ParseErrorEvent extends CustomType {
  constructor(data) {
    super();
    this.data = data;
  }
}
class HandleErrorEvent extends CustomType {
  constructor(method, error) {
    super();
    this.method = method;
    this.error = error;
  }
}
function response_decoder() {
  return field("ok", bool, (ok) => {
    if (ok) {
      return field("value", string2, (value) => {
        return success(new Ok(value));
      });
    } else {
      return field("error", string2, (error) => {
        return success(new Error(error));
      });
    }
  });
}
function convert_js_result(result) {
  let $ = run(result, response_decoder());
  if ($ instanceof Ok) {
    let decoded = $[0];
    return decoded;
  } else {
    let errors = $[0];
    let _pipe = "Failed to decode response: " + join((() => {
      let _pipe2 = errors;
      return map(_pipe2, (e) => {
        return "Expected " + e.expected + " but found " + e.found;
      });
    })(), ", ");
    return new Error(_pipe);
  }
}
function show_message(message) {
  let _pipe = showMessage(message);
  return map_promise(_pipe, convert_js_result);
}
function eval_code(code) {
  let _pipe = evalCode(code);
  return map_promise(_pipe, convert_js_result);
}
function fetch_var(expr, timeout_seconds) {
  let _pipe = fetchVar(expr, timeout_seconds * 1000);
  return map_promise(_pipe, convert_js_result);
}

// build/dev/javascript/emacs_backbone/monadic.mjs
function pure(value) {
  let _pipe = new Ok(value);
  return resolve(_pipe);
}
function bind(cmd, f) {
  return then_await(cmd, (result) => {
    if (result instanceof Ok) {
      let value = result[0];
      return f(value);
    } else {
      let err = result[0];
      let _pipe = new Error(err);
      return resolve(_pipe);
    }
  });
}
function fail_with(error) {
  let _pipe = new Error(error);
  return resolve(_pipe);
}
function lift(res) {
  let _pipe = res;
  return resolve(_pipe);
}
function continue_with(cmd, on_error) {
  return map_promise(cmd, (result) => {
    if (result instanceof Ok) {
      let value = result[0];
      return new Ok(new Ok(value));
    } else {
      let err = result[0];
      return new Ok(new Error(on_error(err)));
    }
  });
}
function run_sequence_loop(remaining_actions, results) {
  if (remaining_actions instanceof Empty) {
    let _pipe = new Ok(reverse(results));
    return resolve(_pipe);
  } else {
    let action = remaining_actions.head;
    let rest = remaining_actions.tail;
    return bind(action, (result) => {
      return run_sequence_loop(rest, prepend(result, results));
    });
  }
}
function run_sequence(actions) {
  return run_sequence_loop(actions, toList([]));
}

// build/dev/javascript/emacs_backbone/emacs.mjs
class EmacsContext extends CustomType {
  constructor(enable_debug, installed_packages_count, get, call, eval$) {
    super();
    this.enable_debug = enable_debug;
    this.installed_packages_count = installed_packages_count;
    this.get = get;
    this.call = call;
    this.eval = eval$;
  }
}
class RawParam extends CustomType {
  constructor(args) {
    super();
    this.args = args;
  }
}
class StringParam extends CustomType {
  constructor(args) {
    super();
    this.args = args;
  }
}
function message(message2) {
  show_message("[Backbone] " + message2);
  return;
}
function get() {
  return (var_name) => {
    return fetch_var(var_name, 5);
  };
}
function format_args(param) {
  if (param instanceof RawParam) {
    let args = param.args;
    let _pipe = args;
    return join(_pipe, " ");
  } else {
    let args = param.args;
    let _pipe = args;
    let _pipe$1 = map(_pipe, string3);
    let _pipe$2 = map(_pipe$1, to_string2);
    return join(_pipe$2, " ");
  }
}
function call_no_return() {
  return (op, param) => {
    let args = format_args(param);
    return eval_code("(" + op + " " + args + ")");
  };
}
function eval_with_return(timeout_seconds) {
  return (op, param) => {
    let args = format_args(param);
    return fetch_var("(" + op + " " + args + ")", timeout_seconds);
  };
}

// build/dev/javascript/emacs_backbone/pkg.mjs
class Branch extends CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
class Tag extends CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
class Version extends CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
class RemoteRecipe extends CustomType {
  constructor(host, repo, ref, files, wait2) {
    super();
    this.host = host;
    this.repo = repo;
    this.ref = ref;
    this.files = files;
    this.wait = wait2;
  }
}
class LocalRecipe extends CustomType {
  constructor(path, files) {
    super();
    this.path = path;
    this.files = files;
  }
}
class Remote extends CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
class Local extends CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
class Pkg extends CustomType {
  constructor(name, recipe, deps, no_compilation) {
    super();
    this.name = name;
    this.recipe = recipe;
    this.deps = deps;
    this.no_compilation = no_compilation;
  }
}

// build/dev/javascript/emacs_backbone/package_tracker.mjs
class PackageTracker extends CustomType {
  constructor(all_installed, pending, installed, total) {
    super();
    this.all_installed = all_installed;
    this.pending = pending;
    this.installed = installed;
    this.total = total;
  }
}
function initialize(packages) {
  let _block;
  let _pipe = packages;
  let _pipe$1 = map(_pipe, (p) => {
    return p.name;
  });
  _block = toArray(_pipe$1);
  let names = _block;
  let _block$1;
  let _pipe$2 = packages;
  let _pipe$3 = map(_pipe$2, (p) => {
    let _block$2;
    let _pipe$32 = p.deps;
    let _pipe$4 = unwrap(_pipe$32, toList([]));
    _block$2 = toArray(_pipe$4);
    let deps = _block$2;
    return [p.name, deps];
  });
  _block$1 = toArray(_pipe$3);
  let deps_entries = _block$1;
  return initializePackageTracker(names, deps_entries);
}
function get_failed_packages() {
  let _pipe = getFailedPackages();
  return to_list(_pipe);
}
function parse_tracker_status(value) {
  let tracker_decoder = field("allInstalled", bool, (all_installed) => {
    return field("pending", list2(string2), (pending) => {
      return field("installed", list2(string2), (installed) => {
        return field("total", int2, (total) => {
          return success(new PackageTracker(all_installed, pending, installed, total));
        });
      });
    });
  });
  let $ = run(value, tracker_decoder);
  if ($ instanceof Ok) {
    let tracker = $[0];
    return tracker;
  } else {
    return new PackageTracker(false, toList([]), toList([]), 0);
  }
}
function update(package_name) {
  let result = updatePackageTracker(package_name);
  return parse_tracker_status(result);
}
function get_package_tracker() {
  let result = getPackageTrackerStatus();
  return parse_tracker_status(result);
}

// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else {
    return alternative();
  }
}
// build/dev/javascript/simplifile/simplifile_js.mjs
import fs from "fs";
import path from "path";
function writeBits(filepath, contents) {
  return gleamResult(() => fs.writeFileSync(path.normalize(filepath), contents.rawBuffer));
}
function isDirectory(filepath) {
  try {
    return new Ok(fs.statSync(path.normalize(filepath)).isDirectory());
  } catch (e) {
    if (e.code === "ENOENT") {
      return new Ok(false);
    } else {
      return new Error(cast_error(e.code));
    }
  }
}
function gleamResult(op) {
  try {
    const val = op();
    return new Ok(val);
  } catch (e) {
    return new Error(cast_error(e.code));
  }
}
function cast_error(error_code) {
  switch (error_code) {
    case "EACCES":
      return new Eacces;
    case "EAGAIN":
      return new Eagain;
    case "EBADF":
      return new Ebadf;
    case "EBADMSG":
      return new Ebadmsg;
    case "EBUSY":
      return new Ebusy;
    case "EDEADLK":
      return new Edeadlk;
    case "EDEADLOCK":
      return new Edeadlock;
    case "EDQUOT":
      return new Edquot;
    case "EEXIST":
      return new Eexist;
    case "EFAULT":
      return new Efault;
    case "EFBIG":
      return new Efbig;
    case "EFTYPE":
      return new Eftype;
    case "EINTR":
      return new Eintr;
    case "EINVAL":
      return new Einval;
    case "EIO":
      return new Eio;
    case "EISDIR":
      return new Eisdir;
    case "ELOOP":
      return new Eloop;
    case "EMFILE":
      return new Emfile;
    case "EMLINK":
      return new Emlink;
    case "EMULTIHOP":
      return new Emultihop;
    case "ENAMETOOLONG":
      return new Enametoolong;
    case "ENFILE":
      return new Enfile;
    case "ENOBUFS":
      return new Enobufs;
    case "ENODEV":
      return new Enodev;
    case "ENOLCK":
      return new Enolck;
    case "ENOLINK":
      return new Enolink;
    case "ENOENT":
      return new Enoent;
    case "ENOMEM":
      return new Enomem;
    case "ENOSPC":
      return new Enospc;
    case "ENOSR":
      return new Enosr;
    case "ENOSTR":
      return new Enostr;
    case "ENOSYS":
      return new Enosys;
    case "ENOBLK":
      return new Enotblk;
    case "ENODIR":
      return new Enotdir;
    case "ENOTSUP":
      return new Enotsup;
    case "ENXIO":
      return new Enxio;
    case "EOPNOTSUPP":
      return new Eopnotsupp;
    case "EOVERFLOW":
      return new Eoverflow;
    case "EPERM":
      return new Eperm;
    case "EPIPE":
      return new Epipe;
    case "ERANGE":
      return new Erange;
    case "EROFS":
      return new Erofs;
    case "ESPIPE":
      return new Espipe;
    case "ESRCH":
      return new Esrch;
    case "ESTALE":
      return new Estale;
    case "ETXTBSY":
      return new Etxtbsy;
    case "EXDEV":
      return new Exdev;
    case "NOTUTF8":
      return new NotUtf8;
    default:
      return new Unknown(error_code);
  }
}

// build/dev/javascript/simplifile/simplifile.mjs
class Eacces extends CustomType {
}
class Eagain extends CustomType {
}
class Ebadf extends CustomType {
}
class Ebadmsg extends CustomType {
}
class Ebusy extends CustomType {
}
class Edeadlk extends CustomType {
}
class Edeadlock extends CustomType {
}
class Edquot extends CustomType {
}
class Eexist extends CustomType {
}
class Efault extends CustomType {
}
class Efbig extends CustomType {
}
class Eftype extends CustomType {
}
class Eintr extends CustomType {
}
class Einval extends CustomType {
}
class Eio extends CustomType {
}
class Eisdir extends CustomType {
}
class Eloop extends CustomType {
}
class Emfile extends CustomType {
}
class Emlink extends CustomType {
}
class Emultihop extends CustomType {
}
class Enametoolong extends CustomType {
}
class Enfile extends CustomType {
}
class Enobufs extends CustomType {
}
class Enodev extends CustomType {
}
class Enolck extends CustomType {
}
class Enolink extends CustomType {
}
class Enoent extends CustomType {
}
class Enomem extends CustomType {
}
class Enospc extends CustomType {
}
class Enosr extends CustomType {
}
class Enostr extends CustomType {
}
class Enosys extends CustomType {
}
class Enotblk extends CustomType {
}
class Enotdir extends CustomType {
}
class Enotsup extends CustomType {
}
class Enxio extends CustomType {
}
class Eopnotsupp extends CustomType {
}
class Eoverflow extends CustomType {
}
class Eperm extends CustomType {
}
class Epipe extends CustomType {
}
class Erange extends CustomType {
}
class Erofs extends CustomType {
}
class Espipe extends CustomType {
}
class Esrch extends CustomType {
}
class Estale extends CustomType {
}
class Etxtbsy extends CustomType {
}
class Exdev extends CustomType {
}
class NotUtf8 extends CustomType {
}
class Unknown extends CustomType {
  constructor(inner) {
    super();
    this.inner = inner;
  }
}
function describe_error(error) {
  if (error instanceof Eacces) {
    return "Permission denied";
  } else if (error instanceof Eagain) {
    return "Resource temporarily unavailable";
  } else if (error instanceof Ebadf) {
    return "Bad file descriptor";
  } else if (error instanceof Ebadmsg) {
    return "Bad message";
  } else if (error instanceof Ebusy) {
    return "Resource busy";
  } else if (error instanceof Edeadlk) {
    return "Resource deadlock avoided";
  } else if (error instanceof Edeadlock) {
    return "Resource deadlock avoided";
  } else if (error instanceof Edquot) {
    return "Disc quota exceeded";
  } else if (error instanceof Eexist) {
    return "File exists";
  } else if (error instanceof Efault) {
    return "Bad address";
  } else if (error instanceof Efbig) {
    return "File too large";
  } else if (error instanceof Eftype) {
    return "Inappropriate file type or format";
  } else if (error instanceof Eintr) {
    return "Interrupted system call";
  } else if (error instanceof Einval) {
    return "Invalid argument";
  } else if (error instanceof Eio) {
    return "Input/output error";
  } else if (error instanceof Eisdir) {
    return "Is a directory";
  } else if (error instanceof Eloop) {
    return "Too many levels of symbolic links";
  } else if (error instanceof Emfile) {
    return "Too many open files";
  } else if (error instanceof Emlink) {
    return "Too many links";
  } else if (error instanceof Emultihop) {
    return "Multihop attempted";
  } else if (error instanceof Enametoolong) {
    return "File name too long";
  } else if (error instanceof Enfile) {
    return "Too many open files in system";
  } else if (error instanceof Enobufs) {
    return "No buffer space available";
  } else if (error instanceof Enodev) {
    return "Operation not supported by device";
  } else if (error instanceof Enolck) {
    return "No locks available";
  } else if (error instanceof Enolink) {
    return "Link has been severed";
  } else if (error instanceof Enoent) {
    return "No such file or directory";
  } else if (error instanceof Enomem) {
    return "Cannot allocate memory";
  } else if (error instanceof Enospc) {
    return "No space left on device";
  } else if (error instanceof Enosr) {
    return "No STREAM resources";
  } else if (error instanceof Enostr) {
    return "Not a STREAM";
  } else if (error instanceof Enosys) {
    return "Function not implemented";
  } else if (error instanceof Enotblk) {
    return "Block device required";
  } else if (error instanceof Enotdir) {
    return "Not a directory";
  } else if (error instanceof Enotsup) {
    return "Operation not supported";
  } else if (error instanceof Enxio) {
    return "Device not configured";
  } else if (error instanceof Eopnotsupp) {
    return "Operation not supported on socket";
  } else if (error instanceof Eoverflow) {
    return "Value too large to be stored in data type";
  } else if (error instanceof Eperm) {
    return "Operation not permitted";
  } else if (error instanceof Epipe) {
    return "Broken pipe";
  } else if (error instanceof Erange) {
    return "Result too large";
  } else if (error instanceof Erofs) {
    return "Read-only file system";
  } else if (error instanceof Espipe) {
    return "Illegal seek";
  } else if (error instanceof Esrch) {
    return "No such process";
  } else if (error instanceof Estale) {
    return "Stale NFS file handle";
  } else if (error instanceof Etxtbsy) {
    return "Text file busy";
  } else if (error instanceof Exdev) {
    return "Cross-device link";
  } else if (error instanceof NotUtf8) {
    return "File not UTF-8 encoded";
  } else {
    let inner = error.inner;
    return "Unknown error: " + inner;
  }
}
function write(filepath, contents) {
  let _pipe = contents;
  let _pipe$1 = bit_array_from_string(_pipe);
  return writeBits(filepath, _pipe$1);
}

// build/dev/javascript/emacs_backbone/resolver.mjs
var FILEPATH = "src/resolver.gleam";

class Node extends CustomType {
  constructor(name, deps, value) {
    super();
    this.name = name;
    this.deps = deps;
    this.value = value;
  }
}
class Cycle extends CustomType {
}
class MissingPackage extends CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
function collect_dependencies(node_map, node) {
  let $ = has_key(node_map, node.name);
  if ($) {
    return node_map;
  } else {
    return insert(node_map, node.name, node);
  }
}
function get_node_map(nodes) {
  return fold2(nodes, new_map(), (acc_map, node) => {
    return collect_dependencies(acc_map, node);
  });
}
function debug_nodes_with_deps(nodes) {
  let _block;
  let _pipe = nodes;
  _block = filter2(_pipe, (node) => {
    let _pipe$1 = node.deps;
    return is_some(_pipe$1);
  });
  let nodes_with_deps = _block;
  return guard(is_empty(nodes_with_deps), undefined, () => {
    let _pipe$1 = nodes_with_deps;
    return each(_pipe$1, (node) => {
      let deps = unwrap(node.deps, toList([]));
      return console_error("  " + node.name + " depends on: " + join(deps, ", "));
    });
  });
}
function build_graph(nodes, enable_debug) {
  console_error("Building dependency graph...");
  let node_map = get_node_map(nodes);
  if (enable_debug) {
    let _pipe = "Node map contains " + inspect2(map_size(node_map)) + " nodes:";
    console_error(_pipe);
    let _pipe$1 = keys(node_map);
    each(_pipe$1, (node_name) => {
      return console_error("  " + node_name);
    });
    console_error("Nodes with dependencies:");
    debug_nodes_with_deps(nodes);
  } else {}
  let graph = try_fold(keys(node_map), new_map(), (acc, node_name) => {
    let curr_graph = acc;
    let $ = map_get(node_map, node_name);
    let node;
    if ($ instanceof Ok) {
      node = $[0];
    } else {
      throw makeError("let_assert", FILEPATH, "resolver", 90, "build_graph", "Pattern match failed, no pattern matched the value.", {
        value: $,
        start: 2467,
        end: 2518,
        pattern_start: 2478,
        pattern_end: 2486
      });
    }
    let _block;
    let _pipe = node.deps;
    let _pipe$1 = unwrap(_pipe, toList([]));
    _block = from_list2(_pipe$1);
    let dep_names = _block;
    let missing_deps = fold3(dep_names, toList([]), (missing, dep_name) => {
      let $1 = has_key(node_map, dep_name);
      if ($1) {
        return missing;
      } else {
        return prepend(dep_name, missing);
      }
    });
    if (missing_deps instanceof Empty) {
      let _pipe$2 = insert(curr_graph, node_name, dep_names);
      return new Ok(_pipe$2);
    } else {
      let _pipe$2 = node_name + " has missing dependencies: " + join(missing_deps, ", ");
      console_error(_pipe$2);
      return new Error(missing_deps);
    }
  });
  if (graph instanceof Ok) {
    let graph$1 = graph[0];
    let final_graph = fold(node_map, graph$1, (g, name, _) => {
      let $ = has_key(g, name);
      if ($) {
        return g;
      } else {
        return insert(g, name, new$());
      }
    });
    return new Ok([final_graph, node_map]);
  } else {
    let missing_deps = graph[0];
    let _pipe = new MissingPackage(missing_deps);
    return new Error(_pipe);
  }
}
function kahn_recursive_step(loop$graph_size, loop$graph, loop$current_in_degree, loop$current_level_nodes, loop$sorted_levels, loop$visited_count) {
  while (true) {
    let graph_size = loop$graph_size;
    let graph = loop$graph;
    let current_in_degree = loop$current_in_degree;
    let current_level_nodes = loop$current_level_nodes;
    let sorted_levels = loop$sorted_levels;
    let visited_count = loop$visited_count;
    if (current_level_nodes instanceof Empty) {
      let $ = visited_count === graph_size;
      if ($) {
        return new Ok(sorted_levels);
      } else {
        let _pipe = "  Visited only " + inspect2(visited_count) + " of " + inspect2(graph_size) + " nodes, but no more nodes to process! Cycle detected";
        console_error(_pipe);
        return new Error(new Cycle);
      }
    } else {
      let current_level_size = length(current_level_nodes);
      let new_visited_count = visited_count + current_level_size;
      let new_sorted_levels = prepend(current_level_nodes, sorted_levels);
      let $ = fold2(current_level_nodes, [current_in_degree, new$()], (acc, node) => {
        let degrees;
        let affected_nodes;
        degrees = acc[0];
        affected_nodes = acc[1];
        let $1 = map_get(graph, node);
        let dependencies;
        if ($1 instanceof Ok) {
          dependencies = $1[0];
        } else {
          throw makeError("let_assert", FILEPATH, "resolver", 203, "kahn_recursive_step", "Pattern match failed, no pattern matched the value.", {
            value: $1,
            start: 5914,
            end: 5965,
            pattern_start: 5925,
            pattern_end: 5941
          });
        }
        return fold3(dependencies, [degrees, affected_nodes], (inner_acc, dep) => {
          let inner_degrees;
          let inner_affected;
          inner_degrees = inner_acc[0];
          inner_affected = inner_acc[1];
          let updated_degrees = upsert(inner_degrees, dep, (maybe_degree) => {
            let current_degree = unwrap(maybe_degree, 0);
            return current_degree - 1;
          });
          let updated_affected = insert2(inner_affected, dep);
          return [updated_degrees, updated_affected];
        });
      });
      let next_in_degree;
      let nodes_with_decremented_in_degree;
      next_in_degree = $[0];
      nodes_with_decremented_in_degree = $[1];
      let next_level_nodes = fold3(nodes_with_decremented_in_degree, toList([]), (nodes, node) => {
        let $1 = map_get(next_in_degree, node);
        if ($1 instanceof Ok) {
          let $2 = $1[0];
          if ($2 === 0) {
            return prepend(node, nodes);
          } else {
            return nodes;
          }
        } else {
          console_error("Warning: Node " + node + " not found in in-degree map");
          return nodes;
        }
      });
      loop$graph_size = graph_size;
      loop$graph = graph;
      loop$current_in_degree = next_in_degree;
      loop$current_level_nodes = next_level_nodes;
      loop$sorted_levels = new_sorted_levels;
      loop$visited_count = new_visited_count;
    }
  }
}
function topological_sort(graph) {
  let _block;
  {
    let all_nodes = keys(graph);
    let initial_in_degrees = fold2(all_nodes, new_map(), (acc, node) => {
      return insert(acc, node, 0);
    });
    _block = fold(graph, initial_in_degrees, (acc_degrees, _, dependencies) => {
      return fold3(dependencies, acc_degrees, (current_degrees, dep_name) => {
        return upsert(current_degrees, dep_name, (maybe_count) => {
          return unwrap(maybe_count, 0) + 1;
        });
      });
    });
  }
  let in_degree = _block;
  let _block$1;
  let _pipe = filter(in_degree, (_, degree) => {
    return degree === 0;
  });
  _block$1 = keys(_pipe);
  let initial_level_nodes = _block$1;
  return kahn_recursive_step(map_size(graph), graph, in_degree, initial_level_nodes, toList([]), 0);
}
function debug_sorted_nodes(sorted_nodes) {
  let _pipe = sorted_nodes;
  index_map(_pipe, (nodes, idx) => {
    return console_error("  " + inspect2(idx) + ": " + (() => {
      let _pipe$1 = nodes;
      let _pipe$2 = map(_pipe$1, (node) => {
        return node.name;
      });
      return join(_pipe$2, " ");
    })());
  });
  return;
}
function resolve_dependencies(nodes, enable_debug) {
  return try$(build_graph(nodes, enable_debug), (_use0) => {
    let graph;
    let node_map;
    graph = _use0[0];
    node_map = _use0[1];
    return try$(topological_sort(graph), (sorted_levels) => {
      let _block;
      let _pipe = sorted_levels;
      _block = map(_pipe, (node_names) => {
        let _pipe$12 = node_names;
        return map(_pipe$12, (node_name) => {
          let $ = map_get(node_map, node_name);
          let node;
          if ($ instanceof Ok) {
            node = $[0];
          } else {
            throw makeError("let_assert", FILEPATH, "resolver", 340, "resolve_dependencies", "Pattern match failed, no pattern matched the value.", {
              value: $,
              start: 10148,
              end: 10199,
              pattern_start: 10159,
              pattern_end: 10167
            });
          }
          return node;
        });
      });
      let sorted_nodes = _block;
      console_error("Dependency resolution complete");
      if (enable_debug) {
        console_error("  Total nodes: " + inspect2((() => {
          let _pipe$12 = sorted_levels;
          let _pipe$2 = flatten(_pipe$12);
          return length(_pipe$2);
        })()));
        console_error("  Number of dependency levels: " + inspect2((() => {
          let _pipe$12 = sorted_nodes;
          return length(_pipe$12);
        })()));
        debug_sorted_nodes(sorted_nodes);
      } else {}
      let _pipe$1 = [sorted_nodes, node_map];
      return new Ok(_pipe$1);
    });
  });
}

// build/dev/javascript/emacs_backbone/pkg_utils.mjs
var FILEPATH2 = "src/pkg_utils.gleam";
function recipe_to_elisp(recipe, pkg) {
  if (recipe instanceof Remote) {
    let r = recipe[0];
    let host_el = ":host " + r.host;
    let repo_el = ':repo "' + r.repo + '"';
    let _block;
    let $ = r.ref;
    if ($ instanceof Branch) {
      let branch = $[0];
      _block = ':branch "' + branch + '"';
    } else if ($ instanceof Tag) {
      let tag = $[0];
      _block = ':tag "' + tag + '"';
    } else {
      let version = $[0];
      _block = ':ref "' + version + '"';
    }
    let ref_el = _block;
    let _block$1;
    let $1 = r.files;
    if ($1 instanceof Some) {
      let files = $1[0];
      _block$1 = `
  :files (` + (() => {
        let _pipe = files;
        let _pipe$1 = map(_pipe, (file) => {
          return '"' + file + '"';
        });
        return join(_pipe$1, " ");
      })() + ")";
    } else {
      _block$1 = "";
    }
    let files_el = _block$1;
    let _block$2;
    let $2 = pkg.no_compilation;
    if ($2 instanceof Some) {
      let $3 = $2[0];
      if ($3) {
        _block$2 = `
  :build (:not elpaca--byte-compile)`;
      } else {
        _block$2 = "";
      }
    } else {
      _block$2 = "";
    }
    let no_compilation_el = _block$2;
    return (() => {
      let _pipe = toList([host_el, repo_el, ref_el]);
      return join(_pipe, " ");
    })() + files_el + no_compilation_el;
  } else {
    let local_recipe = recipe[0];
    let _block;
    let $ = local_recipe.files;
    if ($ instanceof Some) {
      let files = $[0];
      _block = `
  :files (` + (() => {
        let _pipe = files;
        let _pipe$1 = map(_pipe, (file) => {
          return '"' + file + '"';
        });
        return join(_pipe$1, " ");
      })() + ")";
    } else {
      _block = "";
    }
    let files_el = _block;
    let _block$1;
    let $1 = pkg.no_compilation;
    if ($1 instanceof Some) {
      let $2 = $1[0];
      if ($2) {
        _block$1 = `
  :build (:not elpaca--byte-compile)`;
      } else {
        _block$1 = "";
      }
    } else {
      _block$1 = "";
    }
    let no_compilation_el = _block$1;
    return ':repo "' + local_recipe.path + '"' + files_el + no_compilation_el;
  }
}
function generate_packages_el_with_notifications(packages, path2) {
  let pkg_to_elisp = (pkg) => {
    let _block2;
    let $ = pkg.recipe;
    if ($ instanceof Some) {
      let recipe = $[0];
      _block2 = recipe_to_elisp(recipe, pkg);
    } else {
      _block2 = "";
    }
    let recipe_str = _block2;
    let _block$1;
    let $1 = (() => {
      let _pipe2 = recipe_str;
      return trim(_pipe2);
    })();
    if ($1 === "") {
      _block$1 = pkg.name;
    } else {
      let parts = $1;
      _block$1 = "(" + pkg.name + `
  ` + parts + ")";
    }
    let order = _block$1;
    return "(elpaca " + order + `
  (emacs-backbone--reset-package-timeout)
  ;; Notify Backbone once Elpaca finishes this package's queue entry.
  (emacs-backbone--call "package_installed" "` + pkg.name + '"))';
  };
  let _block;
  let _pipe = packages;
  let _pipe$1 = map(_pipe, (pkgs) => {
    let _block$1;
    let _pipe$12 = pkgs;
    let _pipe$22 = map(_pipe$12, pkg_to_elisp);
    _block$1 = join(_pipe$22, `

`);
    let body = _block$1;
    return `(elpaca-queue
` + body + `
)`;
  });
  _block = join(_pipe$1, `

`);
  let packages_definition = _block;
  let _pipe$2 = `(emacs-backbone--begin-package-installation)

` + packages_definition + `

(elpaca-process-queues)
`;
  return write(path2, _pipe$2);
}
function pkg_to_node(pkg) {
  let $ = pkg.deps;
  if ($ instanceof Some) {
    let pkg_deps = $[0];
    return new Node(pkg.name, (() => {
      let _pipe = pkg_deps;
      return new Some(_pipe);
    })(), pkg);
  } else {
    return new Node(pkg.name, new None, pkg);
  }
}
function nodes_to_pacakges(nodes_group, node_map) {
  let _pipe = nodes_group;
  return map(_pipe, (nodes) => {
    let _pipe$1 = nodes;
    return map(_pipe$1, (node) => {
      let _block;
      let _pipe$2 = node_map;
      _block = map_get(_pipe$2, node.name);
      let $ = _block;
      let node$1;
      if ($ instanceof Ok) {
        node$1 = $[0];
      } else {
        throw makeError("let_assert", FILEPATH2, "pkg_utils", 128, "nodes_to_pacakges", "Pattern match failed, no pattern matched the value.", {
          value: $,
          start: 3721,
          end: 3774,
          pattern_start: 3732,
          pattern_end: 3740
        });
      }
      return node$1.value;
    });
  });
}
function genearte_packages(pacakges, path2, enable_debug) {
  let _block;
  let _pipe = pacakges;
  let _pipe$1 = map(_pipe, pkg_to_node);
  _block = resolve_dependencies(_pipe$1, enable_debug);
  let $ = _block;
  let resolved_pkgs;
  let node_map;
  if ($ instanceof Ok) {
    resolved_pkgs = $[0][0];
    node_map = $[0][1];
  } else {
    throw makeError("let_assert", FILEPATH2, "pkg_utils", 14, "genearte_packages", "Pattern match failed, no pattern matched the value.", { value: $, start: 358, end: 494, pattern_start: 369, pattern_end: 399 });
  }
  let _pipe$2 = resolved_pkgs;
  let _pipe$3 = nodes_to_pacakges(_pipe$2, node_map);
  return generate_packages_el_with_notifications(_pipe$3, path2);
}

// build/dev/javascript/emacs_backbone/pkg_macro.mjs
function local_recipe_decoder() {
  return field("local", string2, (local_path) => {
    return optional_field("files", new None, optional(list2(string2)), (files) => {
      let _pipe = new Local(new LocalRecipe(local_path, files));
      return success(_pipe);
    });
  });
}
function remote_recipe_decoder() {
  return field("host", string2, (host) => {
    return field("repo", string2, (repo) => {
      return optional_field("branch", new None, optional(string2), (branch) => {
        return optional_field("tag", new None, optional(string2), (tag) => {
          return optional_field("ref", new None, optional(string2), (version) => {
            return optional_field("files", new None, optional(list2(string2)), (files) => {
              return optional_field("wait", new None, (() => {
                let _pipe = bool;
                return map4(_pipe, (var0) => {
                  return new Some(var0);
                });
              })(), (wait2) => {
                if (branch instanceof Some) {
                  let v = branch[0];
                  let _pipe = new Remote(new RemoteRecipe(host, repo, new Branch(v), files, wait2));
                  return success(_pipe);
                } else if (tag instanceof Some) {
                  let v = tag[0];
                  let _pipe = new Remote(new RemoteRecipe(host, repo, new Tag(v), files, wait2));
                  return success(_pipe);
                } else if (version instanceof Some) {
                  let v = version[0];
                  let _pipe = new Remote(new RemoteRecipe(host, repo, new Version(v), files, wait2));
                  return success(_pipe);
                } else {
                  return failure(new Remote(new RemoteRecipe(host, repo, new Branch(""), files, wait2)), "Remote recipe for repo '" + repo + "' must specify one of: :branch, :tag, or :ref");
                }
              });
            });
          });
        });
      });
    });
  });
}
function recipe_decoder() {
  return one_of(local_recipe_decoder(), toList([remote_recipe_decoder()]));
}
function pkg_decoder() {
  return field("name", string2, (name) => {
    return optional_field("recipe", new None, optional(recipe_decoder()), (recipe) => {
      return optional_field("no-compilation", new None, optional(bool), (no_comp) => {
        return optional_field("deps", new None, optional(list2(string2)), (deps) => {
          let _pipe = new Pkg(name, recipe, deps, no_comp);
          return success(_pipe);
        });
      });
    });
  });
}
function validate_remote_packages(packages) {
  let _block;
  let _pipe = packages;
  _block = filter_map(_pipe, (pkg) => {
    let $ = pkg.recipe;
    if ($ instanceof Some) {
      let $1 = $[0];
      if ($1 instanceof Remote) {
        let $2 = $1[0].ref;
        if ($2 instanceof Branch) {
          let $3 = $2[0];
          if ($3 === "") {
            let repo = $1[0].repo;
            return new Ok([pkg.name, repo]);
          } else {
            return new Error(undefined);
          }
        } else {
          return new Error(undefined);
        }
      } else {
        return new Error(undefined);
      }
    } else {
      return new Error(undefined);
    }
  });
  let invalid = _block;
  if (invalid instanceof Empty) {
    return pure(undefined);
  } else {
    let errors = invalid;
    let _block$1;
    let _pipe$1 = errors;
    let _pipe$2 = map(_pipe$1, (pair) => {
      let name;
      let repo;
      name = pair[0];
      repo = pair[1];
      return "  - " + name + " (repo: " + repo + ")";
    });
    _block$1 = join(_pipe$2, `
`);
    let error_details = _block$1;
    let error_msg = `Remote packages must specify :branch, :tag, or :ref:
` + error_details;
    console_error("[ERROR] " + error_msg);
    message(error_msg);
    return fail_with("Cannot proceed: remote packages missing branch/tag/ref: " + (() => {
      let _pipe$3 = errors;
      let _pipe$4 = map(_pipe$3, (p) => {
        return p[0];
      });
      return join(_pipe$4, ", ");
    })());
  }
}
function validate_local_packages(packages, ctx) {
  let _block;
  let _pipe = packages;
  _block = filter_map(_pipe, (pkg) => {
    let $ = pkg.recipe;
    if ($ instanceof Some) {
      let $1 = $[0];
      if ($1 instanceof Local) {
        let path2 = $1[0].path;
        let $2 = ctx.enable_debug;
        if ($2) {
          console_error("[DEBUG] Validating local package '" + pkg.name + "' at: " + path2);
        } else {}
        let $3 = isDirectory(path2);
        if ($3 instanceof Ok) {
          let $4 = $3[0];
          if ($4) {
            let $5 = ctx.enable_debug;
            if ($5) {
              console_error("[DEBUG] Local package '" + pkg.name + "' path exists: " + path2);
            } else {}
            return new Error(undefined);
          } else {
            console_error("[ERROR] Local package '" + pkg.name + "' path does not exist: " + path2);
            return new Ok([pkg.name, path2]);
          }
        } else {
          console_error("[ERROR] Local package '" + pkg.name + "' path does not exist: " + path2);
          return new Ok([pkg.name, path2]);
        }
      } else {
        return new Error(undefined);
      }
    } else {
      return new Error(undefined);
    }
  });
  let invalid = _block;
  if (invalid instanceof Empty) {
    return pure(undefined);
  } else {
    let errors = invalid;
    let _block$1;
    let _pipe$1 = errors;
    let _pipe$2 = map(_pipe$1, (pair) => {
      let name;
      let path2;
      name = pair[0];
      path2 = pair[1];
      return "  - " + name + ": " + path2;
    });
    _block$1 = join(_pipe$2, `
`);
    let error_details = _block$1;
    let error_msg = `Local package paths do not exist:
` + error_details;
    console_error("[ERROR] " + error_msg);
    message(error_msg);
    return fail_with("Cannot proceed: local package paths do not exist: " + (() => {
      let _pipe$3 = errors;
      let _pipe$4 = map(_pipe$3, (p) => {
        return p[0];
      });
      return join(_pipe$4, ", ");
    })());
  }
}
function extract_index_from_error(errs) {
  let err_str = inspect2(errs);
  let $ = split2(err_str, "path: [");
  if ($ instanceof Empty) {
    return new Error(undefined);
  } else {
    let $1 = $.tail;
    if ($1 instanceof Empty) {
      return new Error(undefined);
    } else {
      let $2 = $1.tail;
      if ($2 instanceof Empty) {
        let rest = $1.head;
        if (rest !== "") {
          let $3 = split2(rest, '"');
          if ($3 instanceof Empty) {
            return new Error(undefined);
          } else {
            let $4 = $3.tail;
            if ($4 instanceof Empty) {
              return new Error(undefined);
            } else {
              let idx_str = $4.head;
              let trimmed = trim(idx_str);
              if (trimmed === "") {
                return new Error(undefined);
              } else {
                let idx = trimmed;
                return new Ok(idx);
              }
            }
          }
        } else {
          return new Error(undefined);
        }
      } else {
        return new Error(undefined);
      }
    }
  }
}
function extract_package_name_from_error(js_data, errs) {
  return try$(extract_index_from_error(errs), (idx_str) => {
    return try$((() => {
      let _pipe = parse_int(idx_str);
      return replace_error(_pipe, undefined);
    })(), (idx) => {
      return try$((() => {
        let _pipe = parse(js_data, list2(dynamic));
        return replace_error(_pipe, undefined);
      })(), (pkgs) => {
        return try$((() => {
          let _pipe = drop(pkgs, idx);
          return first(_pipe);
        })(), (pkg) => {
          let name_decoder = field("name", string2, (name) => {
            return success(name);
          });
          let _pipe = run(pkg, name_decoder);
          return replace_error(_pipe, undefined);
        });
      });
    });
  });
}
function get_decode_error_hint(js_data, errs) {
  let $ = extract_package_name_from_error(js_data, errs);
  if ($ instanceof Ok) {
    let name = $[0];
    return `

[ERROR] Package '` + name + "' has invalid recipe." + `
        Remote packages (:repo) must specify :branch, :tag, or :ref`;
  } else {
    return "";
  }
}
function fetch_packages(ctx) {
  return bind(ctx.eval("emacs-backbone-get-packages", new StringParam(toList([]))), (js_data) => {
    let decode_result2 = parse(js_data, list2(pkg_decoder()));
    if (decode_result2 instanceof Ok) {
      let package_list = decode_result2[0];
      return pure(package_list);
    } else {
      let errs = decode_result2[0];
      let hint = get_decode_error_hint(js_data, errs);
      let error_msg = "Failed to decode list of packages: " + inspect2(errs) + hint;
      console_error("[ERROR] " + error_msg);
      let $ = ctx.enable_debug;
      if ($) {
        console_error("[DEBUG] Raw data: " + js_data);
      } else {}
      return fail_with(error_msg);
    }
  });
}
function generate_macro_defined_packages(ctx, packages_el_path) {
  return bind(fetch_packages(ctx), (pkgs) => {
    return bind(validate_local_packages(pkgs, ctx), (_) => {
      return bind(validate_remote_packages(pkgs), (_2) => {
        let $ = genearte_packages(pkgs, packages_el_path, ctx.enable_debug);
        if ($ instanceof Ok) {
          return pure(pkgs);
        } else {
          let err = $[0];
          let error_msg = "Failed to generate " + packages_el_path + " " + describe_error(err);
          console_error(error_msg);
          return fail_with(error_msg);
        }
      });
    });
  });
}

// build/dev/javascript/emacs_backbone/unit.mjs
class ConfigUnit extends CustomType {
  constructor(name, deps, code) {
    super();
    this.name = name;
    this.deps = deps;
    this.code = code;
  }
}
class FeatureDep extends CustomType {
  constructor(feature) {
    super();
    this.feature = feature;
  }
}
class UnitDep extends CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}
class EnvDep extends CustomType {
  constructor(var_name) {
    super();
    this.var_name = var_name;
  }
}
class ExecutableDep extends CustomType {
  constructor(binary) {
    super();
    this.binary = binary;
  }
}

// build/dev/javascript/emacs_backbone/unit_state.mjs
class UnitState extends CustomType {
  constructor(failed_units, successful_units) {
    super();
    this.failed_units = failed_units;
    this.successful_units = successful_units;
  }
}
function new$2() {
  return new UnitState(new$(), new$());
}
function mark_failed(state, unit_name) {
  return new UnitState(insert2(state.failed_units, unit_name), state.successful_units);
}
function mark_successful(state, unit_name) {
  return new UnitState(state.failed_units, insert2(state.successful_units, unit_name));
}
function is_failed(state, unit_name) {
  return contains(state.failed_units, unit_name);
}

// build/dev/javascript/emacs_backbone/unit_executor.mjs
function get_failed_deps(unit, state, failed_packages) {
  let _pipe = unit.deps;
  let _pipe$1 = unwrap(_pipe, toList([]));
  return filter_map(_pipe$1, (dep) => {
    if (dep instanceof FeatureDep) {
      let feature = dep.feature;
      let $ = contains(failed_packages, feature);
      if ($) {
        return new Ok(feature);
      } else {
        return new Error(undefined);
      }
    } else if (dep instanceof UnitDep) {
      let unit_config_name = dep.name;
      let $ = is_failed(state, unit_config_name);
      if ($) {
        return new Ok(unit_config_name);
      } else {
        return new Error(undefined);
      }
    } else {
      return new Error(undefined);
    }
  });
}
function verify_unit_env_dep(env_var, ctx) {
  return bind(ctx.eval("getenv", new StringParam(toList([env_var]))), (value) => {
    if (value === "") {
      return fail_with("EnvDep failure: " + env_var);
    } else if (value === "null") {
      return fail_with("EnvDep failure: " + env_var);
    } else {
      let $ = ctx.enable_debug;
      if ($) {
        console_error("EnvDep pass: " + env_var);
      } else {}
      return pure("");
    }
  });
}
function verify_unit_executable_dep(binary, ctx) {
  return bind(ctx.eval("executable-find", new StringParam(toList([binary]))), (value) => {
    if (value === "") {
      return fail_with("ExecutableDep failure: " + binary);
    } else if (value === "null") {
      return fail_with("ExecutableDep failure: " + binary);
    } else {
      let $ = ctx.enable_debug;
      if ($) {
        console_error("ExecutableDep pass: " + binary);
      } else {}
      return pure("");
    }
  });
}
function verify_unit_feature_dep(feature, ctx) {
  return bind(fetch_var("(if (require '" + feature + " nil t) t :false)", 5), (value) => {
    if (value === "true") {
      let $ = ctx.enable_debug;
      if ($) {
        console_error("FeatureDep pass: " + feature);
      } else {}
      return pure("");
    } else {
      console_error("FeatureDep failure: " + feature);
      return fail_with("");
    }
  });
}
function verify_runtime_deps_loop(deps, unit, ctx) {
  if (deps instanceof Empty) {
    let $ = ctx.enable_debug;
    if ($) {
      console_error("Unit " + unit.name + " dependency checks passed");
    } else {}
    return pure("");
  } else {
    let dep = deps.head;
    let rest = deps.tail;
    let _block;
    if (dep instanceof FeatureDep) {
      let feature = dep.feature;
      _block = verify_unit_feature_dep(feature, ctx);
    } else if (dep instanceof EnvDep) {
      let env_var = dep.var_name;
      _block = verify_unit_env_dep(env_var, ctx);
    } else if (dep instanceof ExecutableDep) {
      let binary = dep.binary;
      _block = verify_unit_executable_dep(binary, ctx);
    } else {
      _block = pure("");
    }
    let check_result = _block;
    return bind(check_result, (_) => {
      return verify_runtime_deps_loop(rest, unit, ctx);
    });
  }
}
function verify_runtime_deps(unit, ctx) {
  let $ = unit.deps;
  if ($ instanceof Some) {
    let deps = $[0];
    return verify_runtime_deps_loop(deps, unit, ctx);
  } else {
    return pure("");
  }
}
function execute_unit_with_fallback(unit, ctx) {
  let _block;
  let _pipe = verify_runtime_deps(unit, ctx);
  _block = continue_with(_pipe, (err) => {
    return "Runtime dependency failure: " + err;
  });
  let safe_prereq = _block;
  return bind(safe_prereq, (prereq_result) => {
    if (prereq_result instanceof Ok) {
      let $ = unit.code;
      if ($ instanceof Some) {
        let body = $[0];
        let result = fetch_var(body, 5);
        return map_promise(result, (res) => {
          let _block$1;
          if (res instanceof Ok) {
            let $1 = res[0];
            if ($1 === "true") {
              _block$1 = new Ok("");
            } else {
              let err = $1;
              _block$1 = new Error("Execution error: " + err);
            }
          } else {
            _block$1 = res;
          }
          let _pipe$1 = _block$1;
          return new Ok(_pipe$1);
        });
      } else {
        let _pipe$1 = new Ok("Empty body");
        let _pipe$2 = new Ok(_pipe$1);
        return lift(_pipe$2);
      }
    } else {
      let err = prereq_result[0];
      let _pipe$1 = new Error(err);
      let _pipe$2 = new Ok(_pipe$1);
      return lift(_pipe$2);
    }
  });
}
function process_group(loop$units, loop$ctx, loop$state, loop$failed_packages) {
  while (true) {
    let units = loop$units;
    let ctx = loop$ctx;
    let state = loop$state;
    let failed_packages = loop$failed_packages;
    if (units instanceof Empty) {
      return pure(state);
    } else {
      let unit = units.head;
      let rest = units.tail;
      let failed_deps = get_failed_deps(unit, state, failed_packages);
      if (failed_deps instanceof Empty) {
        return bind(execute_unit_with_fallback(unit, ctx), (execute_result) => {
          let _block;
          if (execute_result instanceof Ok) {
            let msg = execute_result[0];
            let $ = ctx.enable_debug;
            if ($) {
              console_error("[SUCCESS] Unit " + unit.name + " executed. " + msg);
            } else {}
            _block = mark_successful(state, unit.name);
          } else {
            let error = execute_result[0];
            console_error("[ERROR] Unit " + unit.name + " failed: " + error);
            _block = mark_failed(state, unit.name);
          }
          let new_state = _block;
          return process_group(rest, ctx, new_state, failed_packages);
        });
      } else {
        let deps = failed_deps;
        let _block;
        let _pipe = map(deps, (dep) => {
          return "'" + dep + "'";
        });
        _block = join(_pipe, ", ");
        let failed_dep_names = _block;
        console_error("[ERROR] Skipping unit '" + unit.name + "' due to failed dependencies: " + failed_dep_names);
        let new_state = mark_failed(state, unit.name);
        loop$units = rest;
        loop$ctx = ctx;
        loop$state = new_state;
        loop$failed_packages = failed_packages;
      }
    }
  }
}
function process_all_groups(groups, ctx, state, failed_packages) {
  if (groups instanceof Empty) {
    return pure(state);
  } else {
    let group = groups.head;
    let rest = groups.tail;
    return bind(process_group(group, ctx, state, failed_packages), (new_state) => {
      return process_all_groups(rest, ctx, new_state, failed_packages);
    });
  }
}
function execute_units(resolved_units, ctx, failed_packages) {
  let initial_state = new$2();
  return bind(process_all_groups(resolved_units, ctx, initial_state, failed_packages), (final_state) => {
    let failed_units = to_list2(final_state.failed_units);
    if (failed_units instanceof Empty) {
      console_error("All units loaded successfully");
      return pure("");
    } else {
      let units = failed_units;
      let failed_names = join(units, ", ");
      console_error("Units loaded with some failures: " + failed_names);
      return pure("");
    }
  });
}

// build/dev/javascript/emacs_backbone/unit_macro.mjs
function unit_decoder() {
  return field("name", string2, (name) => {
    return optional_field("requires", new None, optional(list2(string2)), (features) => {
      return optional_field("after", new None, optional(list2(string2)), (after) => {
        return optional_field("env", new None, optional(list2(string2)), (env) => {
          return optional_field("executable", new None, optional(list2(string2)), (executable) => {
            return optional_field("body", new None, optional(string2), (code) => {
              let _block;
              let _pipe = features;
              let _pipe$1 = unwrap(_pipe, toList([]));
              _block = map(_pipe$1, (var0) => {
                return new FeatureDep(var0);
              });
              let feature_deps = _block;
              let _block$1;
              let _pipe$2 = after;
              let _pipe$3 = unwrap(_pipe$2, toList([]));
              _block$1 = map(_pipe$3, (var0) => {
                return new UnitDep(var0);
              });
              let unit_deps = _block$1;
              let _block$2;
              let _pipe$4 = env;
              let _pipe$5 = unwrap(_pipe$4, toList([]));
              _block$2 = map(_pipe$5, (var0) => {
                return new EnvDep(var0);
              });
              let env_deps = _block$2;
              let _block$3;
              let _pipe$6 = executable;
              let _pipe$7 = unwrap(_pipe$6, toList([]));
              _block$3 = map(_pipe$7, (var0) => {
                return new ExecutableDep(var0);
              });
              let executable_deps = _block$3;
              let _block$4;
              let $ = (() => {
                let _pipe$82 = toList([
                  feature_deps,
                  unit_deps,
                  env_deps,
                  executable_deps
                ]);
                return flatten(_pipe$82);
              })();
              if ($ instanceof Empty) {
                _block$4 = new None;
              } else {
                let value = $;
                _block$4 = new Some(value);
              }
              let deps = _block$4;
              let _pipe$8 = new ConfigUnit(name, deps, code);
              return success(_pipe$8);
            });
          });
        });
      });
    });
  });
}
function fetch_units(ctx) {
  return bind(ctx.eval("emacs-backbone-get-units", new StringParam(toList([]))), (js_data) => {
    let decode_result2 = parse(js_data, optional(list2(unit_decoder())));
    if (decode_result2 instanceof Ok) {
      let package_list = decode_result2[0];
      return pure((() => {
        let _pipe = package_list;
        return unwrap(_pipe, toList([]));
      })());
    } else {
      let errs = decode_result2[0];
      let error_msg = "Failed to decode list of packages: " + inspect2(errs);
      console_error("[ERROR] " + error_msg);
      let $ = ctx.enable_debug;
      if ($) {
        console_error("[DEBUG] Raw data: " + js_data);
      } else {}
      return fail_with(error_msg);
    }
  });
}

// build/dev/javascript/emacs_backbone/unit_utils.mjs
var FILEPATH3 = "src/unit_utils.gleam";
function unit_to_node(unit) {
  let $ = unit.deps;
  if ($ instanceof Some) {
    let unit_deps = $[0];
    let _block;
    let _pipe = unit_deps;
    _block = filter_map(_pipe, (dep) => {
      if (dep instanceof UnitDep) {
        let unit_config_name = dep.name;
        return new Ok(unit_config_name);
      } else {
        return new Error(undefined);
      }
    });
    let unit_only_deps = _block;
    return new Node(unit.name, (() => {
      if (unit_only_deps instanceof Empty) {
        return new None;
      } else {
        let deps = unit_only_deps;
        return new Some(deps);
      }
    })(), unit);
  } else {
    return new Node(unit.name, new None, unit);
  }
}
function nodes_to_units(nodes_group, node_map) {
  let _pipe = nodes_group;
  return map(_pipe, (nodes) => {
    let _pipe$1 = nodes;
    return map(_pipe$1, (node) => {
      let _block;
      let _pipe$2 = node_map;
      _block = map_get(_pipe$2, node.name);
      let $ = _block;
      let node$1;
      if ($ instanceof Ok) {
        node$1 = $[0];
      } else {
        throw makeError("let_assert", FILEPATH3, "unit_utils", 44, "nodes_to_units", "Pattern match failed, no pattern matched the value.", {
          value: $,
          start: 1023,
          end: 1076,
          pattern_start: 1034,
          pattern_end: 1042
        });
      }
      return node$1.value;
    });
  });
}
function validate_unit_after_dependencies(units) {
  let _block;
  let _pipe = units;
  let _pipe$1 = map(_pipe, (unit) => {
    return unit.name;
  });
  _block = from_list2(_pipe$1);
  let known_units = _block;
  let _block$1;
  let _pipe$2 = units;
  _block$1 = filter_map(_pipe$2, (unit) => {
    let _block$2;
    let _pipe$3 = unit.deps;
    let _pipe$4 = unwrap(_pipe$3, toList([]));
    let _pipe$5 = filter_map(_pipe$4, (dep) => {
      if (dep instanceof UnitDep) {
        let name = dep.name;
        let $ = contains(known_units, name);
        if ($) {
          return new Error(undefined);
        } else {
          return new Ok(name);
        }
      } else {
        return new Error(undefined);
      }
    });
    _block$2 = sort(_pipe$5, compare3);
    let missing_after = _block$2;
    if (missing_after instanceof Empty) {
      return new Error(undefined);
    } else {
      let names = missing_after;
      return new Ok([unit.name, names]);
    }
  });
  let invalid_units = _block$1;
  if (invalid_units instanceof Empty) {
    return new Ok(undefined);
  } else {
    let entries = invalid_units;
    let _block$2;
    let _pipe$3 = entries;
    let _pipe$4 = map(_pipe$3, (entry) => {
      let unit_name;
      let missing_after;
      unit_name = entry[0];
      missing_after = entry[1];
      return "  - config-unit '" + unit_name + "' references missing :after unit(s): " + join(missing_after, ", ");
    });
    _block$2 = join(_pipe$4, `
`);
    let details = _block$2;
    return new Error(`Invalid config-unit dependency graph.
` + details + "\n`:after` must reference other config-unit! names, not package! names." + `
Use an existing config unit such as 'minibuffer-settings', or remove the invalid :after entry.`);
  }
}
function resolve_units(packages, enable_debug) {
  return try$(validate_unit_after_dependencies(packages), (_) => {
    let $ = (() => {
      let _pipe = packages;
      let _pipe$1 = map(_pipe, unit_to_node);
      return resolve_dependencies(_pipe$1, enable_debug);
    })();
    if ($ instanceof Ok) {
      let resolve_units$1 = $[0][0];
      let node_map = $[0][1];
      return new Ok((() => {
        let _pipe = resolve_units$1;
        return nodes_to_units(_pipe, node_map);
      })());
    } else {
      let $1 = $[0];
      if ($1 instanceof Cycle) {
        return new Error("Invalid config-unit dependency graph: cycle detected in :after dependencies.");
      } else {
        return new Error("Invalid config-unit dependency graph: unresolved :after dependencies remain.");
      }
    }
  });
}

// build/dev/javascript/emacs_backbone/emacs_backbone.mjs
function unhandle(_, func) {
  let _pipe = "[Backbone] unsupported message: " + func;
  return message(_pipe);
}
function compose_setup(ctx) {
  return toList([
    ctx.call("popwin:popup-buffer", new RawParam(toList(["(get-buffer emacs-backbone-buffer-name)"])))
  ]);
}
function compose_env_injection(ctx) {
  let _pipe = "Starting environment injection";
  console_error(_pipe);
  let result = ctx.call("backbone-load-envvars-file", new StringParam(toList(["~/.config/backbone/env"])));
  let _pipe$1 = "environment injection completed";
  console_error(_pipe$1);
  return result;
}
function compose_packages_configuration(ctx, failed_packages) {
  return bind(fetch_units(ctx), (config_units) => {
    let _block;
    let $ = resolve_units(config_units, ctx.enable_debug);
    if ($ instanceof Ok) {
      let units = $[0];
      _block = pure(units);
    } else {
      let err = $[0];
      _block = fail_with(err);
    }
    let resolved = _block;
    return bind(resolved, (resolved_units) => {
      return bind(execute_units(resolved_units, ctx, failed_packages), (_) => {
        return pure("");
      });
    });
  });
}
function compose_finish(ctx) {
  let _block;
  let _pipe = difference(getInitStartTime(), system_time2());
  let _pipe$1 = to_seconds(_pipe);
  _block = float_to_string(_pipe$1);
  let time_elapsed = _block;
  console_error("Finished in " + time_elapsed + "s");
  let message2 = "Backbone loaded " + to_string(ctx.installed_packages_count) + " packages in " + time_elapsed + "s";
  return bind(ctx.call("run-with-timer", new RawParam(toList([
    "0.5",
    "nil",
    "(lambda () (message " + (() => {
      let _pipe$2 = message2;
      let _pipe$3 = string3(_pipe$2);
      return to_string2(_pipe$3);
    })() + "))"
  ]))), (_) => {
    return pure("");
  });
}
function after_package_installation_handler(ctx, failed_packages) {
  let result = bind(ctx.get("emacs-backbone-enable-debug"), (enable_debug) => {
    return bind((() => {
      let _pipe = new EmacsContext(enable_debug === "true", ctx.installed_packages_count, ctx.get, ctx.call, ctx.eval);
      return compose_packages_configuration(_pipe, failed_packages);
    })(), (_) => {
      return compose_finish(ctx);
    });
  });
  map_promise(result, (res) => {
    if (res instanceof Error) {
      let err = res[0];
      return console_error("[ERROR] Configuration phase failed: " + err);
    } else {
      return;
    }
  });
  return;
}
function handle_notification(context, method, params) {
  if (method === "shutdown") {
    console_error("Received shutdown notification, exiting gracefully");
    return shutdown();
  } else if (method === "package_installed") {
    let name_decoder = field("name", string2, (name) => {
      return success(name);
    });
    let $ = run(params, name_decoder);
    if ($ instanceof Ok) {
      let package_name = $[0];
      let status = update(package_name);
      let $1 = status.all_installed;
      if ($1) {
        return console_error("All packages reported completion, waiting for queue completion");
      } else {
        return;
      }
    } else {
      return console_error("Failed to decode package_installed params: " + inspect2(params));
    }
  } else if (method === "packages_finished") {
    let reason_decoder = optional_field("reason", new None, optional(string2), (reason2) => {
      return success(reason2);
    });
    let _block;
    let $ = run(params, reason_decoder);
    if ($ instanceof Ok) {
      let value = $[0];
      _block = value;
    } else {
      _block = new None;
    }
    let reason = _block;
    if (reason instanceof Some) {
      let value = reason[0];
      console_error("Packages finished (" + value + "), continuing with configuration");
    } else {
      console_error("Packages finished, continuing with configuration");
    }
    let status = get_package_tracker();
    let failed_packages = get_failed_packages();
    if (failed_packages instanceof Empty) {} else {
      let names = failed_packages;
      let pending_count = status.total - length(status.installed);
      let _block$1;
      if (reason instanceof Some) {
        let value = reason[0];
        _block$1 = " (reason: " + value + ")";
      } else {
        _block$1 = "";
      }
      let reason_text = _block$1;
      console_error("[WARN] Package processing ended before all packages reported Backbone completion" + reason_text + ".");
      console_error("[WARN] This does not necessarily mean every listed package failed to install;" + " it means Backbone never saw their `package_installed` callback.");
      console_error("[WARN] Pending packages: " + to_string(pending_count) + "/" + to_string(status.total) + ". Pending packages plus transitive dependents: " + join(names, ", "));
    }
    return after_package_installation_handler(new EmacsContext(context.enable_debug, status.total, context.get, context.call, context.eval), from_list2(failed_packages));
  } else {
    return unhandle(context, method);
  }
}
function compose_packages_installation(ctx) {
  let _pipe = "Starting package installation";
  console_error(_pipe);
  return bind(ctx.eval("make-temp-file", new StringParam(toList(["emacs-backbone"]))), (packages_el_path) => {
    let _block;
    let _pipe$1 = packages_el_path;
    _block = replace(_pipe$1, '"', "");
    let packages_el_path$1 = _block;
    return bind(generate_macro_defined_packages(ctx, packages_el_path$1), (package_list) => {
      initialize(package_list);
      let $ = is_empty(package_list);
      if ($) {
        console_error("No packages declared, continuing with configuration");
        after_package_installation_handler(new EmacsContext(ctx.enable_debug, 0, ctx.get, ctx.call, ctx.eval), new$());
        return pure("");
      } else {
        return eval_with_return(10 * 60)("load-file", new StringParam(toList([packages_el_path$1])));
      }
    });
  });
}
function init(ctx, request_id) {
  return bind(ctx.get("emacs-backbone-enable-debug"), (enable_debug) => {
    let enable_debug_flag = enable_debug === "true";
    enableDebug(enable_debug_flag);
    let ctx$1 = new EmacsContext(enable_debug_flag, ctx.installed_packages_count, ctx.get, ctx.call, ctx.eval);
    return bind(run_sequence((() => {
      let $ = ctx$1.enable_debug;
      if ($) {
        return compose_setup(ctx$1);
      } else {
        return toList([]);
      }
    })()), (_) => {
      return bind(run_sequence(toList([
        compose_env_injection(ctx$1),
        compose_packages_installation(ctx$1)
      ])), (_2) => {
        sendResponse(request_id, "ok");
        return pure("");
      });
    });
  });
}
function handle_request(context, id, method, _) {
  if (method === "init") {
    updateInitStartTime(system_time2());
    init(context, id);
    return;
  } else {
    unhandle(context, method);
    return sendErrorResponse(id, -32601, "Unknown method: " + method);
  }
}
function handler(context) {
  return (event) => {
    if (event instanceof RequestEvent) {
      let id = event.id;
      let method = event.method;
      let params = event.params;
      handle_request(context, id, method, params);
      return;
    } else if (event instanceof NotificationEvent) {
      let method = event.method;
      let params = event.params;
      handle_notification(context, method, params);
      return;
    } else if (event instanceof ParseErrorEvent) {
      let data = event.data;
      return console_error("Failed to parse data: " + data);
    } else {
      let method = event.method;
      let error = event.error;
      return console_error("Failed to handle " + method + ", error: " + error);
    }
  };
}
function main() {
  let context = new EmacsContext(false, 0, get(), call_no_return(), eval_with_return(5));
  setupStdioServer(handler(context));
  return console_error("Backbone stdio server started");
}

// build/dev/javascript/emacs_backbone/gleam@@private_main_v1.15.4.mjs
main();
