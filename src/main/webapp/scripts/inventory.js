"use strict";

// require storefinder!

// Global variable, singleton inventory
// Tasked to obtain inventory of products at LCBO site when offered from back end a list of products with inventories
// some of which could be stale, most notably when it's stated as 0, the only condition for which we update with a new value
// from LCBO (otherwise we trust back end and reduce load on queries to LCBO).
var inventory = (function() {

  var storeId = null;
  var prodIdCheckboxes = {}; // map of prodId to html checkbox elements having the name containing the prodId <input type=checkbox... name=prodId ...>

  var prodIdNodeKey = function(prodNodeElt){
    return prodNodeElt.value;
  };

  var prodIdCBQuantityBuddy = function(el, prodId) {
    // server (ProductInteraction) sets name to prodId for easy look up and pairing.
    return el.parents().find('td[name=' + prodId + ']');
  };

  var ajaxGetInventories = function(storeId, productId) {
    var success = function(data, status){
      var currProdId = data.result.product_id;
      var theCheckBox = prodIdCheckboxes[currProdId];
      if (theCheckBox != undefined) {
        var quantity = data.result.quantity;
        prodIdCBQuantityBuddy($(theCheckBox), currProdId).html(quantity);
      }
    };

    var error = function(data, status){  // deliberate avoidance of callback hell with much nesting being avoided
      console.log('Error Data: ' + data.responseText + '\nStatus: ' + status );
    };

    var uri = 'http://lcboapi.com/stores/' + storeId + '/products/' + productId + '/inventory'
    $.ajax({
      url: uri,
      dataType: 'jsonp', // for Cross Origin Access
      type: 'GET',
      success: success,
      error: error
    });
  };

  var getInventories = function() {
    prodIdCheckboxes[prodIdNodeKey(this)] = this;
    var productId = parseInt(this.value) || 0;
    var quantityEl = prodIdCBQuantityBuddy($(this), productId);
    var quantity = parseInt($(quantityEl).val()) || 0;
    if (storeId > 0 && productId > 0 && quantity <= 0) { // browser gets an update to complete likely missing info
      ajaxGetInventories(storeId, productId);
    }
  }

  return {
    fetchInventories: function() {
      prodIdCheckboxes = {}; // reset it. Too bad, if there was an earlier request. Just take current user input.
      storeId = storeFinder.getTheSelectedLcboStoreId(); // calls for requirejs or ES6 module usage perhaps
      $('div.prodContainer').find('input:checkbox').each(getInventories);
    }
  }
}());



