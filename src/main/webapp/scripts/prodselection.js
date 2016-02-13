"use strict";

// Global variable, singleton prodSelection
var prodSelection = (function() {
  return {
    currentProds: function(data) {
      var selectedProdIds = [];
      var checkedElements = $("div.prodContainer").find("input:checked");
      for (var i = 0; i < checkedElements.length; i++) {
        selectedProdIds.push(parseInt(checkedElements[i].value)); // this is the lcbo id of the product
      }
      return JSON.stringify(selectedProdIds);
    }
  }
}());



