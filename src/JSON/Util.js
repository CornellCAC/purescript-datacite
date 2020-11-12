"use strict";

exports.tryPrettyJson = function (jString) {
  var jsPretty = jString;
  return function() {
    if (jString === undefined) return null;

    var jsPrettyMay = JSON.stringify(JSON.parse(jString), undefined, 2);
    if (jsPrettyMay === undefined || jsPrettyMay === null) {
      jsPretty = jString;
    } else {
      jsPretty = jsPrettyMay;
    } 
    return jsPretty;
  };
};