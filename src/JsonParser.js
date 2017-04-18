"use strict";

exports.toForeignImpl = function (left, right, str) {
  try {
    return right(JSON.parse(str));
  } catch (e) {
    return left(e.toString());
  }
};
