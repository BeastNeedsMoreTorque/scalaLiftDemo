"use strict";

var toggleImage = (function() {
    var thickBorder = 'thickBorder';
    var thinBorder = 'thinBorder';

    var toggleSelect = function(eltSelector, newClass, oldClass) {
        $(eltSelector).removeClass(oldClass).addClass(newClass);  // changes the border style between two alternatives
    };

    return {
        // adds a selected thick frame around selected img element and sets thin frame around the deselected/old one.
        frameRadioImage: function(container, newSelection) {
            var images = $("#"+container).find("img").get(); // they are img descendants of the container
            var oldSelection = images.find(function(selector){
                return $(selector).hasClass(thickBorder);
            });
            if (typeof oldSelection != 'undefined') toggleSelect(oldSelection, thinBorder, thickBorder);
            toggleSelect($("[name="+newSelection+"]"), thickBorder, thinBorder);
        }
    }
}());

