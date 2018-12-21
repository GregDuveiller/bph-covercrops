
var gpp = ee.ImageCollection("MODIS/006/MYD17A2H"),
    albedo = ee.ImageCollection("MODIS/006/MCD43A3"),
    arable = ee.FeatureCollection("users/guidolavespa2511/LUCAS_ARABLE"),
    GlobCover = ee.Image("ESA/GLOBCOVER_L4_200901_200912_V2_3"),
    CORINE = ee.Image("users/guidolavespa2511/CORINE_2012"),
    Reflectances = ee.ImageCollection("MODIS/006/MCD43A4"),
    BRDF_QUALITY = ee.ImageCollection("MODIS/006/MCD43A2");

// PsnNet	kg*C/m^2	
// scale =0.0001	

// Albedo
// scale: 0.001



var SNR_Timeseries = ee.Image('users/gduveiller/spHet/SNR4Lugato_coll_2003to2017')

print(SNR_Timeseries)



var landcover = GlobCover.select('landcover');
Map.addLayer(landcover)
// print(albedo.first().getInfo())

// var start_date = '2008-10-01';
// var end_date = '2009-09-30'


Map.addLayer(arable,{color:'00FF00'},'points LUCAS');
var arable_buffer = arable.geometry().buffer(5000);


var listOfBands = ['sig_variance', 'sig_variance_1', 'sig_variance_2', 'sig_variance_3', 'sig_variance_4', 'sig_variance_5',
'sig_variance_6', 'sig_variance_7', 'sig_variance_8', 'sig_variance_9', 'sig_variance_10','sig_variance_11'
, 'sig_variance_12','sig_variance_13', 'sig_variance_14'] 


var i = 0
// print(listOfBands[i])


for (var y=2003; y<2017; y++) {


var start_date = ee.Date.fromYMD(y, 10, 1);
var end_date   = ee.Date.fromYMD(y+1, 9, 30);
print(end_date)

var SNR = SNR_Timeseries.select(listOfBands[i])

i = i+1
print(i)
print(listOfBands[i])

////1st part: SNR

///Load Palette
var palette1 = ['FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718',
               '74A901', '66A000', '529400', '3E8601', '207401', '056201',
               '004C00', '023B01', '012E01', '011D01', '011301'];
// Map.addLayer(  SNR, {min: 0, max: 30, palette: palette1}, 'SNR_2009');



////2nd part: GPP -> select only desidered layers, i.e. PsnNet and apply a quality mask
var PsnNet_filtered = gpp.select('PsnNet','Psn_QC').filterDate(start_date,end_date);

// print(PsnNet)
// Bitmask for Psn_QC
//         Bit 0: MODLAND QC bits
//         0: Good quality
//         1: Other quality
//               Bit 1: Sensor
//         0: Terra
//         1: Aqua
//               Bit 2: Dead detector
//         0: Detectors apparently fine for up to 50% of channels 1, 2
//         1: Dead detectors caused >50% adjacent detector retrieval
//               Bits 3-4: Cloud state
//         0: Significant clouds NOT present (clear)
//         1: Significant clouds WERE present
//         2: Mixed cloud present on pixel
//         3: Cloud state not defined, assumed clear
//               Bits 5-7: 5-level confidence quality score
//         0: Very best possible
//         1: Good, very usable, but not the best
//         2: Substandard due to geometry problems - use with caution
//         3: Substandard due to other than geometry problems - use with caution
//         4: Couldn't retrieve pixel (NOT PRODUCED AT ALL - non-terrestrial biome)
//         7: Fill Value


////2nd part B : Reflectances -> select only desidered layers, i.e. PsnNet and apply a quality mask
var Reflect = Reflectances.select('Nadir_Reflectance_Band1','Nadir_Reflectance_Band2',
            'BRDF_Albedo_Band_Mandatory_Quality_Band1','BRDF_Albedo_Band_Mandatory_Quality_Band2').filterDate(start_date,end_date);


// calculate indices -- only NDVI
function addNDVI(image) {
  var ndvi = image.normalizedDifference(['Nadir_Reflectance_Band2', 'Nadir_Reflectance_Band1']);
  ndvi = ndvi.multiply(255).byte()
  return image.addBands(ndvi.rename(['NDVI']))
    //.addBands(image.metadata('system:time_start'));
}


// Map the NDVI function over the collection.
Reflect = Reflect
  .map(addNDVI);

Reflect = Reflect.select('NDVI','BRDF_Albedo_Band_Mandatory_Quality_Band1')

// print(Reflect)


////3rd part: albedo -> select only desidered layers, i.e. ??? and apply a quality mask
var albedo_shw = albedo.
select('Albedo_BSA_shortwave','Albedo_WSA_shortwave','BRDF_Albedo_Band_Mandatory_Quality_shortwave').
filterDate(start_date,end_date);

var addmeanBands = function(image) {
 
 var b1 = image.select('Albedo_BSA_shortwave');
  var b2 = image.select('Albedo_WSA_shortwave');
 var Mean_A = (b1.add(b2)).divide(2)
 
   return image.addBands(Mean_A.rename(['AlbedoMean']));
    //addBands(image.metadata('system:time_start'));
};


// Map the average albedo function over the collection.
var albedo_shw_filtered_mean = albedo_shw
  .map(addmeanBands);

albedo_shw_filtered_mean = albedo_shw_filtered_mean.select('AlbedoMean','BRDF_Albedo_Band_Mandatory_Quality_shortwave')

// print(albedo_shw_filtered_mean)


//// ALBEDO QUALITY FLAGS

var BRDF_QUAL = BRDF_QUALITY.select('Snow_BRDF_Albedo').filterDate(start_date,end_date);

// print(BRDF_QUAL)


////5th part: extract


// var gpp_first = ee.Image(gpp_2009.first()).updateMask(snr.gte(10));//.select(['PsnNet']);
// print(gpp_first)
// print(arable.limit(15))
// Map.addLayer(  gpp_first)




var computeCSV = function(image) {
  
// Sample 21x21 patches (10+1+10).
var labeledPatches = image.updateMask(CORINE.gte(12).and(CORINE.lte(22)))//.updateMask(snr.gte(10))
  .neighborhoodToArray(ee.Kernel.square(10))
  .sampleRegions({
    collection: arable,
    projection: 'EPSG:4326',
    scale: 500
  });
  
  return labeledPatches;
};


// print(PsnNet_filtered)
// print(albedo_shw_filtered_mean)

var albedo_shw_filtered_CSV = albedo_shw_filtered_mean.map(computeCSV).flatten();
var gpp_filtered_CSV = PsnNet_filtered.map(computeCSV).flatten();
var Reflect_CSV = Reflect.map(computeCSV).flatten();
var BRDF_CSV = BRDF_QUAL.map(computeCSV).flatten();



// Reflect

// print(albedo_shw_filtered_CSV.limit(70))


// print(BRDF_CSV.limit(70))


var labeledPatches_SNR = SNR.updateMask(CORINE.gte(12).and(CORINE.lte(22)))//.updateMask(snr.gte(10))
  .neighborhoodToArray(ee.Kernel.square(10))
  .sampleRegions({
    collection: arable,
    projection: 'EPSG:4326',
    scale: 500
  });

print(labeledPatches_SNR.limit(3))
print(gpp_filtered_CSV.limit(3))
print(Reflect_CSV.limit(3))
print(albedo_shw_filtered_CSV.limit(3))
print('BRDF_CSV is',BRDF_CSV.limit(3))



////6th part: export
// Export to a csv file on Google Drive
Export.table.toDrive({
  collection: albedo_shw_filtered_CSV,
    //collection: ['sur_refl_b01', 'sur_refl_b02', 'sur_refl_b03', 'sur_refl_b04', 'sur_refl_b05', 'sur_refl_b06', 'sur_refl_b07'], //labeledPatches,
  description: 'LabeledPatchesalbedo'+ y,
  folder: 'LUCAS-test',
  fileNamePrefix: 'LabeledPatchesalbedo'+ y,
      selectors: ['system:index','POINT_ID','sample_ID','GPS_LAT','GPS_LONG','AlbedoMean','BRDF_Albedo_Band_Mandatory_Quality_shortwave']

  // fileFormat: 'TFRecord',
  // selectors: ['sur_refl_b01', 'sur_refl_b02', 'sur_refl_b03', 'sur_refl_b04', 'sur_refl_b05', 'sur_refl_b06', 'sur_refl_b07'],
});

Export.table.toDrive({
  collection: gpp_filtered_CSV,
    //collection: ['sur_refl_b01', 'sur_refl_b02', 'sur_refl_b03', 'sur_refl_b04', 'sur_refl_b05', 'sur_refl_b06', 'sur_refl_b07'], //labeledPatches,
  description: 'LabeledPatchesgpp'+ y,
  folder: 'LUCAS-test',
  fileNamePrefix: 'LabeledPatchesgpp'+ y,
  // fileFormat: 'TFRecord',PsnNet
    selectors: ['system:index','POINT_ID','sample_ID','GPS_LAT','GPS_LONG','PsnNet']

  // selectors: ['sur_refl_b01', 'sur_refl_b02', 'sur_refl_b03', 'sur_refl_b04', 'sur_refl_b05', 'sur_refl_b06', 'sur_refl_b07'],
});

Export.table.toDrive({
  collection: Reflect_CSV,
    //collection: ['sur_refl_b01', 'sur_refl_b02', 'sur_refl_b03', 'sur_refl_b04', 'sur_refl_b05', 'sur_refl_b06', 'sur_refl_b07'], //labeledPatches,
  description: 'LabeledPatchesReflect'+ y,
  folder: 'LUCAS-test',
  fileNamePrefix: 'LabeledPatchesReflect'+ y,
      selectors: ['system:index','POINT_ID','sample_ID','GPS_LAT','GPS_LONG','NDVI']

  // fileFormat: 'TFRecord',
  // selectors: ['sur_refl_b01', 'sur_refl_b02', 'sur_refl_b03', 'sur_refl_b04', 'sur_refl_b05', 'sur_refl_b06', 'sur_refl_b07'],
}); 



Export.table.toDrive({
  collection: labeledPatches_SNR,
    //collection: ['sur_refl_b01', 'sur_refl_b02', 'sur_refl_b03', 'sur_refl_b04', 'sur_refl_b05', 'sur_refl_b06', 'sur_refl_b07'], //labeledPatches,
  description: 'LabeledPatchesSNR'+ y,
  folder: 'LUCAS-test',
  fileNamePrefix: 'LabeledPatchesSNR'+ y
  // fileFormat: 'TFRecord',
  // selectors: ['sur_refl_b01', 'sur_refl_b02', 'sur_refl_b03', 'sur_refl_b04', 'sur_refl_b05', 'sur_refl_b06', 'sur_refl_b07'],
});


Export.table.toDrive({
  collection: BRDF_CSV,
    //collection: ['sur_refl_b01', 'sur_refl_b02', 'sur_refl_b03', 'sur_refl_b04', 'sur_refl_b05', 'sur_refl_b06', 'sur_refl_b07'], //labeledPatches,
  description: 'LabeledPatchesQUAL'+ y,
  folder: 'LUCAS-test',
  fileNamePrefix: 'LabeledPatchesQUAL'+ y
  // fileFormat: 'TFRecord',
  // selectors: ['sur_refl_b01', 'sur_refl_b02', 'sur_refl_b03', 'sur_refl_b04', 'sur_refl_b05', 'sur_refl_b06', 'sur_refl_b07'],
});

}
Map.addLayer(arable_buffer,{color:'0000FF'},'points LUCAS buffer');

