"use strict";
// shim for ECMA-262, http://www.tutorialspoint.com/javascript/array_foreach.htm
if (!Array.prototype.forEach)
{
   Array.prototype.forEach = function(fun, thisp)
   {
      var len = this.length;
      if (typeof fun != "function")
      throw new TypeError();

      var thisp = arguments[1];
      for (var i = 0; i < len; i++)
      {
         if (i in this)
         fun.call(thisp, this[i], i, this); // here, we don't really use thisp, but we might eventually need it.
      }
   };
}

// Global variable, singleton lcboViewer
// API:
// <localhost:port>/store/lat/<lat>/lon/<lon> gives closest store from coordinate(lat,lon).
// Currently supported Store  (see lcboapi.com): id, name, is_dead, latitude, longitude, distance_in_meters (from specified location), address_line_1, city
var lcboViewer = (function() {
    var imgSelectStyle = 'selectframe';
    var imgUnSelectStyle = 'unselectframe';

    var defaultOntarioLocationLat= 43.647219;  // Bay & Front LCBO address.
    var defaultOntarioLocationLon= -79.3789987;

    var fetchStore = function(lat, lon, userLocationAvailable) {
        // Show particulars of nearby store in storeNearby and also show that point in a google map.
        $.ajax({
            // liftweb interprets periods in special way for suffixes in JSON calls (req parsing), so we escape them by using commas instead.
            url: '/store/lat/' + lat.toString().replace('.',  ',') + '/lon/' + lon.toString().replace('.', ','),
            type: 'GET',
            success: function(data, status){
                var latlon = new google.maps.LatLng(data.latitude, data.longitude)
                var mapholder = document.getElementById('mapholder')
                mapholder.style.height = '250px';
                mapholder.style.width = '250px';

                var myOptions = {
                    center:latlon,zoom:14,
                    mapTypeId:google.maps.MapTypeId.HYBRID,
                    mapTypeControl:true
                }
                var map = new google.maps.Map(mapholder, myOptions);
                var title = 'Downtown Toronto Liquor Store';
                if (userLocationAvailable) {
                    // #storeNearby: You are XYZ.NN kms from branch <name> located at <address> in <city>
                    var distOutput = (data.distance_in_meters/1000.0).toFixed(2) + ' kms';
                    $("#storeNearby").html('Closest LCBO store:');
                    title = "Closest Liquor Store!";
                    $("#storeName").html(data.name);
                    $("#storeAddressLine1").html(data.address_line_1);
                    $("#storeCity").html(data.city);
                    $("#storeDistance").html(distOutput);
                    $("#storeLat").html(data.latitude);
                    $("#storeLon").html(data.longitude);
                    $("#storeAttributesTbl").show();
                } else {
                    $("#storeNearby").html("Downtown Toronto LCBO (user location unavailable)");
                    $("#storeAttributesTbl").hide();
                }
                var marker = new google.maps.Marker({position:latlon,map:map,title:title});
            },
            error: function(data, status){
                console.log("Error Data: " + data + "\nStatus: " + status );
                alert("Unable to locate nearest store from specified location" );
            }
        });
     };
    var fetchStoreNearUser = function(position) {
        fetchStore(position.coords.latitude, position.coords.longitude, true);
     };

     var showGeoError = function(error) {
         fetchStore(defaultOntarioLocationLat, defaultOntarioLocationLon, false);
     };

    return {
        fetchStoreFromPosition: function() {
            if (navigator.geolocation) {
                navigator.geolocation.getCurrentPosition(fetchStoreNearUser, showGeoError);
            } else  {
                $("#storeNearby").html("Geolocation is not supported by this browser.");
                fetchStore(defaultOntarioLocationLat, defaultOntarioLocationLon, false);
            }
        },

        frameRadioImage: function(container, selected) {
            var imgElts = $("#"+container).find("img").get();
            imgElts.forEach(function(element){
                if ($(element).attr('name') != selected) {
                   $(element).attr('class', imgUnSelectStyle);
                }
            });
            $("[name="+selected+"]").attr('class', imgSelectStyle);
        }
    }
}());

$(window).load(function(){
    lcboViewer.fetchStoreFromPosition();
});


