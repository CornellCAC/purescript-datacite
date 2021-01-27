"use strict";

let unsafeIsOk = function (val) {
  return !( typeof val === 'undefined' || val === null );
}

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
  if (unsafeIsOk(jsObj.data)
      && unsafeIsOk(jsObj.data.attributes)
      &&  unsafeIsOk(jsObj.data.attributes.xml)
      ) {
        jsObj['data']['attributes']['xml'] = undefined;
        var strOut = JSON.stringify(jsObj);
        if (strOut === undefined || strOut === null) {
          return "";
        } else {
          return strOut
        }
      };
};
