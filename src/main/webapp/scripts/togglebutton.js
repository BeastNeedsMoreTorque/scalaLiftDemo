"use strict";

var toggleButton = (function() {
  var thickBorderCls = 'thickBorder';
  var thinBorderCls = 'thinBorder';

  var toggleSelect = function(eltSelector, newClass, oldClass) {
    $(eltSelector).removeClass(oldClass).addClass(newClass);  // changes the border style between two alternatives
  };

  return {
    // adds a selected thick frame around selected element and sets thin frame around the deselected/old one.
    frame: function(container, newSelection) {
      var nodes = $("#"+container).find('*').get(); // they are descendants of the container
      var oldSelection = nodes.find(function(selector){
        return $(selector).hasClass(thickBorderCls);
      });
      if (typeof oldSelection != 'undefined') toggleSelect(oldSelection, thinBorderCls, thickBorderCls);

      toggleSelect($("[name="+newSelection+"]"), thickBorderCls, thinBorderCls);
    }
  }
}());

