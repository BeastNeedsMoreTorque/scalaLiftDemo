"use strict";

// Global variable, singleton prodSelection
var prodSelection = (function() {
  var parseCurrency = function(currency) {
      var number = Number(currency.replace(/[^0-9\.]+/g,""));
      return parseFloat(number) || 0.0;
  };

  var formatAsCurrency = function(number) {
    return "$" + number.toFixed(2); // cheats a lot...
  };

  return {
    updateItem: function(data) {
      var siblings = $(data).parent().siblings(); // need to go around label.

      var fixedCostEl = $(siblings).parent().find("input.hiddenProdCost");
      var cost = parseCurrency($(fixedCostEl).val());

      var quantityEl = $(siblings).find("input.prodQty");
      var qty = parseInt($(quantityEl).val()) || 0;

      if (qty > 0 && data.checked) {
        cost = cost * qty;
      }
      var effectiveCostEl = $(siblings).find("input.prodCost");
      $(effectiveCostEl).val(formatAsCurrency(cost));
    },

    currentProds: function(data) {
      var selectedProdIds = [];
      var selectedItems = [];
      $("div.prodContainer").find("input:checked").each(function() {
        var lcbo_id = parseInt(this.value) || 0;

        var siblings = $(this).parent().siblings();

        var quantityEl = $(siblings).find("input.prodQty");
        var quantity = parseInt($(quantityEl).val()) || 0;

        var costEl = $(siblings).find("input.prodCost");
        var cost = parseCurrency($(costEl).val());

        var data = {
          lcbo_id: lcbo_id,
          quantity: quantity,
          cost: cost
        };
        selectedItems.push(data);
        selectedProdIds.push(lcbo_id); // this is the lcbo id of the product
      });
      return JSON.stringify(selectedItems);
    }
  }
}());



