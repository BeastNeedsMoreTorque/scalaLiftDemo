"use strict";

// Global variable, singleton lcboViewer
// API:
// <localhost:port>/store/lat/<lat>/lon/<lon> gives closest store from coordinate(lat,lon).
// Currently supported Store  (see lcboapi.com): id, name, is_dead, latitude, longitude, distance_in_meters (from specified location), address_line_1, city
var lcboViewer = (function() {
    var defaultOntarioLocationLat= 43.647219;  // Bay & Front LCBO address.
    var defaultOntarioLocationLon= -79.3789987;

    var mapCanvas = document.getElementById('map-canvas')
    var map;
    var closestStoreName = 'Unknown Store';
    var fetchStore = function(lat, lon, userLocationAvailable) {
        var userLatLon = new google.maps.LatLng(lat, lon)
        var myOptions = {
            center:userLatLon,zoom:11,
            mapTypeId:google.maps.MapTypeId.HYBRID,
            mapTypeControl:true
        }
        map = new google.maps.Map(mapCanvas, myOptions);
        var userMarker = new google.maps.Marker({position:userLatLon,map:map,title:"Current Location",icon:"http://maps.google.com/mapfiles/ms/icons/green-dot.png"});
        // Show particulars of nearby store in storeNearby and also show that point in a google map.
        $.ajax({
            // liftweb interprets periods in special way for suffixes in JSON calls (req parsing), so we escape them by using commas instead.
            url: '/store/lat/' + lat.toString().replace('.',  ',') + '/lon/' + lon.toString().replace('.', ','),
            type: 'GET',
            success: function(data, status){
                var latlon = new google.maps.LatLng(data.latitude, data.longitude)
                var title = 'Downtown Toronto Liquor Store';
                if (userLocationAvailable) {
                    var distOutput = (data.distance_in_meters/1000.0).toFixed(2) + ' kms';
                    $("#storeNearby").html('Closest LCBO store:');
                    title = "Closest Liquor Store!";
                    $("#storeName").html(data.name);
                    closestStoreName = data.name;
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
                var closestMarker = new google.maps.Marker({position:latlon,map:map,title:title,icon:"http://maps.google.com/mapfiles/ms/icons/blue-dot.png"});
                fetchStores(map.getBounds(), data.name);
            },
            error: function(data, status){
                console.log("Error Data: " + data.responseText + "\nStatus: " + status );
                alert(data.responseText );
            }
        });
    };

    var fetchStores = function(bounds) {
        var viewSouthWest = bounds.getSouthWest();
        var viewNorthEast = bounds.getNorthEast();
        var url = '/stores/lat1/' + viewSouthWest.lat().toString().replace('.',  ',') + '/lon1/' + viewSouthWest.lng().toString().replace('.', ',')
                              + '/lat2/' + viewNorthEast.lat().toString().replace('.', ',')+ '/lon2/' + viewNorthEast.lng().toString().replace('.', ',');
        $.ajax({
            url: url,
            type: 'GET',
            success: function(data, status){
                function createMarker(element, index, array) {
                    if (element.name != closestStoreName) {
                        var latlon = new google.maps.LatLng(element.latitude, element.longitude);
                        var marker = new google.maps.Marker({position:latlon,map:map});
                    }
                }
                data.forEach(createMarker);
            },
            error: function(data, status){
                console.log("Error Data: " + data.responseText + "\nStatus: " + status );
                alert(data.responseText );
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
        }
    }
}());

$(document).ready(function(){
    lcboViewer.fetchStoreFromPosition();
});


