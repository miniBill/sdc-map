// Written in 2014-2016 by Dmitry Chestnykh and Devi Mandiri.
// Public domain.
"use strict";

export function decodeUTF8(s) {
  if (typeof s !== "string") throw new TypeError("expected string");
  var i,
    d = unescape(encodeURIComponent(s)),
    b = new Uint8Array(d.length);
  for (i = 0; i < d.length; i++) b[i] = d.charCodeAt(i);
  return b;
}

export function encodeBase64(arr) {
  var i,
    s = [],
    len = arr.length;
  for (i = 0; i < len; i++) s.push(String.fromCharCode(arr[i]));
  return btoa(s.join(""));
}
