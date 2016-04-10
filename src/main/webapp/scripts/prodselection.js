"use strict";

// Global variable, singleton prodSelection
var prodSelection = (function() {
  var selectedItems = [];

  var parseCurrency = function(currency) {
    var number = Number(currency.replace(/[^0-9\.]+/g,""));
    return parseFloat(number) || 0.0;
  };

  var getUnitCost = function(ancestorElt) {
    var unitCostEl = ancestorElt.find('input.hiddenProdCost');
    return parseCurrency($(unitCostEl).val());
  };

  var formatAsCurrency = function(number) {
    return '$' + number.toFixed(2); // cheats a lot on internationalization (but this is Ontario!)...
  };

  var collectSelectedProductDetails = function() {
    var id = parseInt(this.value) || 0;

    var siblings = $(this).parent().siblings();

    var quantityEl = $(siblings).find('input.prodQty');
    var desiredQuantity = parseInt($(quantityEl).val()) || 0;

    var availableQtyEl = $(siblings).parent().find('input.hiddenProdInv');
    var quantity = Math.min(desiredQuantity, parseInt($(availableQtyEl).val()) || 0);
    var missedQty = desiredQuantity - quantity;

    var transactCost = quantity * getUnitCost($(siblings).parent());

    var data = {
      id: id,
      quantity: quantity,
      cost: transactCost,
      missedQty: missedQty
    };
    selectedItems.push(data);
  };

  return {
    updateQtyItem: function(data) {
      var qty = parseInt(data.value) || 0;
      if (qty < 1) {  // don't let user enter garbage numeric there, enforce it's >= 1
        qty = 1;
        $(data).val(1);
      }
      var siblings = $(data).parent().siblings(); // need to go around label.
      var transactCost = $(siblings).find('input.prodCost');
      var unitCost = getUnitCost($(siblings).parent());
      $(transactCost).val(formatAsCurrency(unitCost * qty));
    },

    currentProds: function() {
      selectedItems = [];
      $('div.prodContainer').find('input:checked').each(collectSelectedProductDetails);
      return JSON.stringify(selectedItems);
    }
  }
}());



