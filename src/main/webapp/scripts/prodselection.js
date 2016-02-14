"use strict";

// Global variable, singleton prodSelection
var prodSelection = (function() {
  return {
    updateItem: function(data) {
      var siblings = $(data).parent().siblings(); // need to go around label.

      var fixedCostEl = $(siblings).parent().find("input.hiddenProdCost");
      var cost = parseFloat($(fixedCostEl).val()) || 0.0;

      var quantityEl = $(siblings).find("input.prodQty");
      var qty = parseInt($(quantityEl).val()) || 0;

      if (qty > 0 && data.checked) {
        cost = cost * qty;
      }
      var effectiveCostEl = $(siblings).find("input.prodCost");
      $(effectiveCostEl).val(cost.toFixed(2));

    },

    currentProds: function(data) {
      var selectedProdIds = [];
      var selectedItems = [];
      $("div.prodContainer").find("input:checked").each(function() {
        var siblings = $(this).parent().siblings();
        var quantityEl = $(siblings).find("input.prodQty");
        var costEl = $(siblings).find("input.prodCost");

        var lcbo_id = parseInt(this.value) || 0;
        var quantity = parseInt($(quantityEl).val()) || 0;
        var cost = parseFloat($(costEl).val()) || 0.0;

        var data = {
          lcbo_id: lcbo_id,
          quantity: quantity,
          cost: cost
        };
        selectedItems.push(data);
        selectedProdIds.push(lcbo_id); // this is the lcbo id of the product
      });
      //console.log(JSON.stringify(selectedItems));
      return JSON.stringify(selectedProdIds);
    }
  }
}());



