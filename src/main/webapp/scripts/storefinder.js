"use strict";

// Global variable, singleton storeFinder
// API:
// <localhost:port>/stores gives us all stores with coordinates and we evaluate the closest.
// Uses Google maps API quite a bit.
var storeFinder = (function() {
  // Bay & Front LCBO address items.
  var defaultOntarioLocation;
  var DEFAULT_ON_LAT=43.647219;
  var DEFAULT_ON_LNG=-79.3789987;

  var KMS_PER_LAT = 111; // good approximation at 45' of latitude, which mostly works in Ontario (undefined if not near Ontario).
  var KMS_PER_LNG = 78.4;

  var GREEN_MARKER_ICON_URI = 'http://maps.google.com/mapfiles/ms/icons/green-dot.png';
  var BLUE_MARKER_ICON_URI = 'http://maps.google.com/mapfiles/ms/icons/blue-dot.png';
  var CURR_LOC_MARKER_TITLE = 'Current Location';
  var CLOSEST_STORE = 'Closest store';
  var SELECTED_STORE = 'Selected LCBO store:';
  var DEFAULT_STORE_LOCATION_ERR = 'Downtown Toronto LCBO (user location unavailable)';
  var GEO_LOC_UNAVAILABLE = 'Geolocation is not supported by this browser.';

  var ZOOM_LOWER_BOUND = 10;
  var STD_ZOOM = 12;

  var distMatrixService;
  var directionsService;
  var directionsDisplay;

  var mapCanvas = document.getElementById('map-canvas');
  var map;
  var markers = [];
  var stores = [];
  var userMarker;
  var userLocation;
  var storeDistance;  // could also get storeDuration if we wanted.
  var mapOptions = {};

  var theSelectedStore = null;

  // evaluate distance ourselves because of 25 destination distances limit in Google API, we have more than 600. Some hack.
  var distanceByGeo = function(latLng1, latLng2) {
    var x = KMS_PER_LAT * (latLng1.lat()-latLng2.lat());  // reliable
    var y = KMS_PER_LNG * (latLng1.lng()-latLng2.lng());  // approximately reliable at 45 North, which is typical Ontario (don't care about users out of continent).
    return (Math.sqrt(Math.pow(x,2) + Math.pow(y,2)));
  };

  var closestStore = function(latLng) {
    var bestDistance = +Infinity;
    var closest = null;
    stores.forEach(
      function (store) {
        var storeLatLng = new google.maps.LatLng(store.latitude, store.longitude)
        var dist = distanceByGeo(latLng, storeLatLng);
        if (dist < bestDistance) {
          closest = store;
          bestDistance = dist;
        }
      }
    );
    if (closest !== null && theSelectedStore == null) {
      flipMarker(closest.name);
    }
    return closest;
  }

  var flipMarker = function(name) {
    markers.forEach(
      function (marker, index, array) {
        if (marker.title.localeCompare(name) == 0) {
          marker.setIcon(BLUE_MARKER_ICON_URI);
        }
      }
    );
  }

  var fetchStore = function(userLatLng) {
    theSelectedStore = closestStore(userLatLng);
    if (theSelectedStore !== null) {
      var closestLatLng = new google.maps.LatLng(theSelectedStore.latitude, theSelectedStore.longitude)
      evaluateDistance(closestLatLng);
      getDirections(closestLatLng);

      $('#storeNearby').html(SELECTED_STORE);
      $('#storeName').html(theSelectedStore.name);
      $('#storeAddressLine1').html(theSelectedStore.address_line_1);
      $('#storeCity').html(theSelectedStore.city);
      $('#storeLat').html(theSelectedStore.latitude);
      $('#storeLon').html(theSelectedStore.longitude);
      $('#storeAttributesTbl').show();
    } else {
      $('#storeNearby').html(DEFAULT_STORE_LOCATION_ERR);
      $('#storeAttributesTbl').hide();
    }
  };

  var evaluateDistance = function(latLng) {
    distMatrixService.getDistanceMatrix(
    {
      origins: [userLocation],
      destinations: [latLng],
      travelMode: google.maps.TravelMode.DRIVING
    }, storeFinder.distMatrixCB);
  };

  var directionsListener = function(result, status) {
    if (status == google.maps.DirectionsStatus.OK) {
      directionsDisplay.setMap(null);
      directionsDisplay.setMap(map);
      directionsDisplay.setDirections(result);
    }
  };

  var getDirections = function(latLng) {
   var request = {
      origin: userLocation,
      destination: latLng,
      travelMode: google.maps.TravelMode.DRIVING
    };
    directionsService.route(request, directionsListener);
  };

  var fetchAllStores = function() {
    $.ajax({
      url: '/stores',
      type: 'GET',
      success: function(data, status){
        function createMarker(element, index, array) {
          var latlng = new google.maps.LatLng(element.latitude, element.longitude);
          addMarker(element.name, latlng);
        }
        data.forEach(createMarker);
        stores = data;
        fetchStore( userLocation);
      },
      error: function(data, status){
        console.log('Error Data: ' + data.responseText + '\nStatus: ' + status );
        alert(data.responseText );
      }
    });
  };

  var boundsChangedListener = function() {
    clearMarkers();
    if (this.zoom > ZOOM_LOWER_BOUND) {
      showMarkers();
    }
    else {
      clearMarkers();
    }
  };

  var userLocationCallback = function(position) {
    distMatrixService = new google.maps.DistanceMatrixService();
    directionsService = new google.maps.DirectionsService();

    userLocation = new google.maps.LatLng(position.coords.latitude, position.coords.longitude);
    mapOptions = {
      center:userLocation,
      zoom:STD_ZOOM,
      mapTypeId:google.maps.MapTypeId.HYBRID,
      mapTypeControl:true
    }
    map = new google.maps.Map(mapCanvas, mapOptions);
    map.addListener('bounds_changed', boundsChangedListener);
    var rendererOptions = {
      suppressMarkers: true
    }

    directionsDisplay = new google.maps.DirectionsRenderer(rendererOptions);
    directionsDisplay.setMap(map);
    userMarker = new google.maps.Marker(
      {position:userLocation,
       map:map,
       title:CURR_LOC_MARKER_TITLE,
       icon:GREEN_MARKER_ICON_URI});
    fetchAllStores();
    $(mapCanvas).removeAttr( 'hidden' );
  };

  var showGeoError = function(error) {
    defaultOntarioLocation = new google.maps.LatLng(DEFAULT_ON_LAT, DEFAULT_ON_LNG);
    userLocation = defaultOntarioLocation;
    fetchStore(defaultOntarioLocation);
  };

  // Adds a marker to the map and push to the array.
  var addMarker = function(name, location) {
    var marker = new google.maps.Marker({
      title: name,
      position: location,
      map: map
    });
    marker.addListener('click', storeFinder.storeClickCB);
    markers.push(marker);
  };

  // Sets the map on all markers in the array.
  var setMapOnAll = function (map) {
    markers.forEach( function(marker) {
      marker.setMap(map);
    });
  };

  // Removes the markers from the map, but keeps them in the array.
  var clearMarkers= function() {
    setMapOnAll(null);
  };

  // Shows any markers currently in the array.
  var showMarkers = function() {
    setMapOnAll(map);
  };

  // Deletes all markers in the array by removing references to them.
  var deleteMarkers = function() {
    clearMarkers();
    markers = [];
  };

  return {
    storeClickCB: function(e) {
      fetchStore(e.latLng);
      evaluateDistance(e.latLng);
      getDirections(e.latLng);
    },

    getTheSelectedStore: function() {
      if (theSelectedStore == null) return -1;
      return theSelectedStore.idField;
    },

    getTheSelectedLcboStoreId: function() {
      if (theSelectedStore == null) return -1;
      return theSelectedStore.lcbo_id;
    },

    distMatrixCB: function(response, status) {
      if (status == google.maps.DistanceMatrixStatus.OK) {
        var origins = response.originAddresses;
        if(origins.length > 0) {
          var results = response.rows[0].elements;
          if(results.length > 0 ) {
            var element = results[0];
            storeDistance = element.distance.text;
            $('#storeDistance').html(storeDistance);
          }
        }
      }
    },

    initMap: function() {
      if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(userLocationCallback, showGeoError);
      } else  {
        $('#storeNearby').html(GEO_LOC_UNAVAILABLE);
        fetchStore(defaultOntarioLocation);
        $(mapCanvas).removeAttr( 'hidden' );
      }
    }
  }
}());



