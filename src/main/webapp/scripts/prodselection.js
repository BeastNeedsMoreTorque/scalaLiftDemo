"use strict";

// Global variable, singleton prodSelection
var prodSelection = (function() {
  return {
    currentProds: function(data) {
      var selectedProds = [];
      var checkedElements = $("div.prodContainer").find("input:checked");
      for (var i = 0; i < checkedElements.length; i++) {
        selectedProds.push(checkedElements[i].value);
      }
      return JSON.stringify(selectedProds);
    }
  }
}());



