var mymap = L.map('mapid', {
    // minZoom: 12,
    // maxZoom: 5,
}).setView([38.9, -77], 9);

L.tileLayer('https://api.mapbox.com/styles/v1/{id}/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYWFkaXR0YW1iZSIsImEiOiJja3V4NmllOHo0dHY0MnVvZnhib3BhcDZtIn0.fX-8GmIEb5jh8r_MBb8J9w', {
    maxZoom: 18,
    attribution: 'Map data &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, ' +
        'Imagery © <a href="https://www.mapbox.com/">Mapbox</a>',
    id: 'mapbox/light-v10',
    tileSize: 512,
    zoomOffset: -1,
}).addTo(mymap);


// L.circle([38.9865102, -76.9464573], 1000, {
//     color: 'red',
//     fillColor: '#f03',
//     fillOpacity: 0.5
// }).addTo(mymap).bindPopup("I am a <a href='google.com'>polygon</a>.");

L.marker([38.9865102, -76.9464573]).addTo(mymap)
    .bindPopup("<b>University of Maryland, College Park</b><br /><a href='google.com'>View data</a>").openPopup();

L.marker([38.9076089, -77.0722585]).addTo(mymap)
    .bindPopup("<b>Georgetown University</b><br /><a href='google.com'>View data</a>").openPopup();

L.marker([38.8997145, -77.0507879]).addTo(mymap)
    .bindPopup("<b>George Washington University</b><br /><a href='google.com'>View data</a>").openPopup();

L.marker([38.9226843, -77.0216264]).addTo(mymap)
    .bindPopup("<b>Howard University</b><br /><a href='google.com'>View data</a>").openPopup();

L.marker([39.3925121, -76.6148279]).addTo(mymap)
    .bindPopup("<b>Towson University</b><br /><a href='google.com'>View data</a>").openPopup();

L.marker([38.3453519, -75.6083267]).addTo(mymap)
    .bindPopup("<b>Salisbury University</b><br /><a href='google.com'>View data</a>").openPopup();

L.marker([38.8298118, -77.3095493]).addTo(mymap)
    .bindPopup("<b>George Mason University</b><br /><a href='google.com'>View data</a>").openPopup();

L.marker([39.2556759, -76.7109674]).addTo(mymap)
    .bindPopup("<b>University of Maryland, Baltimore County</b><br /><a href='google.com'>View data</a>").openPopup();


var popup = L.popup();

// function onMapClick(e) {
//     popup
//         .setLatLng(e.latlng)
//         .setContent("You clicked the map at " + e.latlng.toString())
//         .openOn(mymap);
// }

// mymap.on('click', onMapClick);