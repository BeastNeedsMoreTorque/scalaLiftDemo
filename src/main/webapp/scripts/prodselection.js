"use strict";

// Global variable, singleton prodSelection

var prodSelection = (function() {
  return {
    clear: function() {
      $("#chosenProds").val(JSON.stringify([]));
    },

    checkBoxCB: function(data) {
      var selectedProds = [];
      var checkedElements = $("div.prodContainer").find("input:checked");
      for (var i = 0; i < checkedElements.length; i++) {
        selectedProds.push(checkedElements[i].value);
      }
      $("#chosenProds").val(JSON.stringify(selectedProds));
    }
  }
}());



