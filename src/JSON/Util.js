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

exports.preParse = function (jString) {
  var jsObj = JSON.parse(jString)
  jsObj['data']['attributes']['xml'] = undefined;
  var strOut = JSON.stringify(jsObj);
  if (strOut === undefined || strOut === null) {
    return "";
  } else {
    return strOut
  } 
};

exports.field = function (fieldName) {
  return function (fObj) {
    return fObj[fieldName];
  };
};
