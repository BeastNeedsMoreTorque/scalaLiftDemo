"use strict";

// Global variable, singleton prodSelection
var prodSelection = (function() {
  var selectedItems = [];
  var parseCurrency = function(currency) {
      var number = Number(currency.replace(/[^0-9\.]+/g,""));
      return parseFloat(number) || 0.0;
  };

  var formatAsCurrency = function(number) {
    return '$' + number.toFixed(2); // cheats a lot on internationalization (but this is Ontario!)...
  };

  var collectSelectedProductDetails = function() {
    var id = parseInt(this.value) || 0;

    var siblings = $(this).parent().siblings();

    var quantityEl = $(siblings).find('input.prodQty');
    var desiredQuantity = parseInt($(quantityEl).val()) || 0;

    var fixedCostEl = $(siblings).parent().find('input.hiddenProdCost');
    var availableQtyEl = $(siblings).parent().find('input.hiddenProdInv');
    var quantity = Math.min(desiredQuantity, parseInt($(availableQtyEl).val()) || 0);
    var missedQty = desiredQuantity - quantity;

    var cost = quantity * parseCurrency($(fixedCostEl).val());

    var data = {
      id: id,
      quantity: quantity,
      cost: cost,
      missedQty: missedQty
    };
    selectedItems.push(data);
  }

  return {
    updateQtyItem: function(data) {
      var siblings = $(data).parent().siblings(); // need to go around label.

      var fixedCostEl = $(siblings).parent().find('input.hiddenProdCost');
      var cost = parseCurrency($(fixedCostEl).val());
      var qty = parseInt(data.value) || 0;
      if (qty < 1) {
        qty = 1;
        $(data).val(1);
      }

      var effectiveCostEl = $(siblings).find('input.prodCost');
      $(effectiveCostEl).val(formatAsCurrency(cost * qty));
    },

    currentProds: function() {
      selectedItems = [];
      $('div.prodContainer').find('input:checked').each(collectSelectedProductDetails);
      return JSON.stringify(selectedItems);
    }
  }
}());



