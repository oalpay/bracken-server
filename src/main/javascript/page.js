$(function () {
    $('.image-upload-form').each(function(key,form){
        $(form).fileupload({
            dataType: 'json',
            add: function (e,data) {
                $(form).find('.progress .bar').css('width', '0%');
                $(form).find('.progress .bar').show();
                data.submit();
            },
            progressall: function (e, data) {
                var progress = parseInt(data.loaded / data.total * 100, 10) + '%';
                $(form).find('.progress .bar').css('width', progress);
            },
            done: function (e, data) {
                $(form).find('.progress .bar').fadeOut(1000);
                $(form).find('img').attr('src',data.result.name);
            }
        });
    });
    var map;
    var marker;
    $('#pageMapModal').on('shown.bs.modal', function () {
        var pageLocation = new google.maps.LatLng($('#pageLatitude').val(), $('#pageLongitude').val());
        if(!map){
            var mapOptions = {
                zoom: 12,
                center: pageLocation,
                mapTypeId: google.maps.MapTypeId.ROADMAP
            };
            map = new google.maps.Map(document.getElementById('mapCanvas'),mapOptions);
            marker = new google.maps.Marker({ position: pageLocation, map: map});
            google.maps.event.addListener(map, 'click', function(event) {
                marker.setPosition(event.latLng);
            });
            $('#mapLocationSave').click(function(){
                   var position = marker.getPosition();
                   $('#pageLatitude').val(position.lat());
                   $('#pageLongitude').val(position.lng());
            });
            var geocoder = new google.maps.Geocoder();
            var geocode = function(){
              $('#geocoderSearch').button('loading');
              geocoder.geocode({'address': $('#geocoderAddress').val()} , function(results, status){
                  $('#geocoderSearch').button('reset');
                  if (status == google.maps.GeocoderStatus.OK){
                      var location = results[0].geometry.location;
                      map.setCenter(location);
                  }
              });
            }
            $('#geocoderSearch').click(function(){
                geocode();
            });
            $('#geocoderAddress').keypress(function(event) {
                if (event.keyCode == 13) {
                    geocode();
                }
            });

        }
        marker.setPosition(pageLocation);
        map.setCenter(marker.getPosition());

    });
	
	$(".timeago").timeago();
});