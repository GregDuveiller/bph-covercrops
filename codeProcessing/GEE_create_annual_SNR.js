
var MOD09GA_col = ee.ImageCollection("MODIS/006/MOD09GA"),
    MYD09GA_col = ee.ImageCollection("MODIS/006/MYD09GA"),
    ROI = /* color: #d63000 */ee.Geometry.Polygon(
        [[[-12.65625, 36.4566360115962],
          [0.3515625, 34.16181816123038],
          [17.2265625, 34.16181816123038],
          [31.2890625, 33.72433966174761],
          [34.8046875, 34.30714385628804],
          [34.8046875, 37.3002752813443],
          [32.51953125, 45.089035564831036],
          [32.81812232740435, 54.2070147532921],
          [30.76171875, 57.79794388498275],
          [32.87109375, 66.01801815922043],
          [31.9921875, 67.5421666883853],
          [9.66796875, 67.13582938531948],
          [2.4609375, 62.83508901142282],
          [-7.20703125, 60.06484046010452],
          [-14.4140625, 54.36775852406841],
          [-12.83203125, 46.92025531537451]]]);




// // Define Area of Interest 
// Map.addLayer(AOI);   
    
/**
 * Returns an image containing just the specified QA bits.
 *
 * Args:
 *   image - The QA Image to get bits from.
 *   start - The first bit position, 0-based.
 *   end   - The last bit position, inclusive.
 *   name  - A name for the output image.
 */ // function to get the MODIS QA flags
var getQABits = function(image, start, end, newName) {
    // Compute the bits we need to extract.
    var pattern = 0;
    for (var i = start; i <= end; i++) {
       pattern += Math.pow(2, i);
    }
    return image.select([0], [newName])
                  .bitwiseAnd(pattern)
                  .rightShift(start);
};

// function to mask out based on flags 
var maskPixels = function(image0) {
  // Select the QA band
  var QA = image0.select('state_1km');

  // Get the land_water_flag bits.
  var landWaterFlag = getQABits(QA, 3, 5, 'land_water_flag');

  // Get the cloud_state bits and find cloudy areas.
  var cloud = getQABits(QA, 0, 1, 'cloud_state').expression("b(0) == 1 || b(0) == 2");
  // Get the cloud_shadow bit
  var cloudShadows = getQABits(QA, 2, 2, 'cloud_shadow');
  // Get the Pixel is adjacent to cloud bit
  var cloudAdjacent = getQABits(QA, 13, 13, 'cloud_adj');
  // Get the internal cloud flag
  var cloud2 = getQABits(QA, 10, 10, 'cloud_internal');
                
  // Get the internal fire flag
  var fire = getQABits(QA, 11, 11, 'fire_internal');

  // Get the MOD35 snow/ice flag
  var snow1 = getQABits(QA, 12, 12, 'snow_MOD35');
  // Get the internal snow flag
  var snow2 = getQABits(QA, 15, 15, 'snow_internal');

  // Create a mask that filters out undesired areas
  var mask = landWaterFlag.eq(1)
            .and(cloud.not()).and(cloudShadows.not()).and(cloudAdjacent.not()).and(cloud2.not())
            .and(fire.not())
            .and(snow1.not()).and(snow2.not());

  return image0.updateMask(mask);
 // return mask;
}

// Use this function to add variables for NDVI
var addNDVI = function(image) {
  // Return the image with the added bands.
  return image.addBands(image.normalizedDifference(['sur_refl_b02', 'sur_refl_b01']).rename('NDVI')).float();
  };


//---- SET PARAMS ----
// set the name of the ROI
// make sure an asset collection with the same name exists under /spHet/
var roiName = 'Europa';
// set the width of the temporal smoothing bandwidth
var bw = 21;
//--------------------


// annual composites
var annual_snr= ee.List.sequence(2003,2017,1).map(function(year){
  year = ee.Number(year)
  
  var start_date = ee.Date.fromYMD(year,10,1);
  var end_date = ee.Date.fromYMD(year.add(1),9,30);
  
  
  
// var start_date = ee.Date.fromYMD(2008,10,1);
// var end_date = ee.Date.fromYMD(2009,9,30);

var MOD09 = MOD09GA_col.filterDate(start_date, end_date).select('sur_refl_b02', 'sur_refl_b01','state_1km');
var MYD09 = MYD09GA_col.filterDate(start_date, end_date).select('sur_refl_b02', 'sur_refl_b01','state_1km');

var MOD09masked = MOD09.map(maskPixels);
var MYD09masked = MYD09.map(maskPixels);

// This field contains UNIX time in milliseconds.
var timeField = 'system:time_start';

// Mask flags
var MOD09ndvi = MOD09masked.map(addNDVI).select('NDVI');
var MYD09ndvi = MYD09masked.map(addNDVI).select('NDVI');

// Merge AQUA and TERRA
var MCD09ndvi = ee.ImageCollection(MOD09ndvi.merge(MYD09ndvi));
var filteredMODIS = MCD09ndvi.sort("system:time_start");

// Smoothing
var join = ee.Join.saveAll({
  matchesKey: 'images'
});
var diffFilter = ee.Filter.maxDifference({
  difference: 1000 * 60 * 60 * 24 * bw,
  leftField: timeField, 
  rightField: timeField
});
var threeNeighborJoin = join.apply({
  primary: filteredMODIS, 
  secondary: filteredMODIS, 
  condition: diffFilter
});
var smoothed = ee.ImageCollection(threeNeighborJoin.map(function(image) {
  var collection = ee.ImageCollection.fromImages(image.get('images'));
  return ee.Image(image).addBands(collection.mean().rename('mean'));
}));


var var_obs = smoothed.map(function(image){
  return image.select('NDVI').rename('obs');
}).reduce(ee.Reducer.variance());

var var_sig = smoothed.map(function(image){
  return image.select('mean').rename('sig');
}).reduce(ee.Reducer.variance());

var var_res = smoothed.map(function(image){
  return image.select('NDVI').rename('res').subtract(image.select('mean'));
}).reduce(ee.Reducer.variance());


var snr = var_sig.divide(var_res);



  
  return snr
  
})
  

print(annual_snr)

var annual_snr_coll = ee.ImageCollection.fromImages(annual_snr)

print(annual_snr_coll)


Map.addLayer(annual_snr_coll.first(),{min:0,max:30})

// convert image collection to a multi-band image
var annual_snr_IMG = ee.Image(annual_snr_coll.iterate(function (newImage, img) {
  return ee.Image(img).addBands(newImage)}, ee.Image([])))

print(annual_snr_IMG)






Export.image.toAsset({
  image: annual_snr_IMG.multiply(2.5).byte(),
  assetId: 'users/gduveiller/spHet/SNR4Lugato',
  region: ROI, 
  scale:463.312716528,
  crs:'SR-ORG:6974',
//  crsTransform:imRef4crs.projection().transform(),
  description: 'SNR_500m_Europe',
  maxPixels: 1e13
});


