
var arable = ee.FeatureCollection("users/guidolavespa2511/LUCAS_ARABLE");

// // test client/server side
// var test_1 = ee.Number(1)
// print(test_1)
// var test_2 = test_1.add(1)
// print(test_2)




Map.addLayer(arable,{color:'00FF00'},'points LUCAS');
// var arable_buffer = arable.geometry().buffer(45);


function bufferFeature(ft){
  ft = ft.buffer(45)
    return ft.set({ncoords: ft.geometry().coordinates().length()})
}

var arable_buffer = arable.map(bufferFeature)
print(arable_buffer.limit(2))

print(arable.limit(2))

///Load Palette
var palette1 = ['FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718',
               '74A901', '66A000', '529400', '3E8601', '207401', '056201',
               '004C00', '023B01', '012E01', '011D01', '011301'];
var start_date = '2003-01-01';
var end_date = '2017-12-31'



// var collection = L5_TOC.
// filterDate(start_date,end_date)

var L5coll = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
.select('B[1-5]','pixel_qa')
.filterDate(start_date,end_date)
.filterMetadata('IMAGE_QUALITY', 'equals', 9)

var L7coll = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR')
.select('B[1-5]','pixel_qa')
.filterDate(start_date,end_date)
.filterMetadata('IMAGE_QUALITY', 'equals', 9)


var L8coll_raw = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR')
.select('B[2-6]','pixel_qa')
.map(function(image){
  return image.rename(['B1', 'B2', 'B3', 'B4', 'B5', 'pixel_qa']);
});

// print(L8coll_raw.limit(3))

var L8coll = L8coll_raw.select('B[1-5]','pixel_qa')
.filterDate(start_date,end_date)
.filterMetadata('IMAGE_QUALITY_OLI', 'equals', 9)

// print(L8coll.limit(3))


var collection = ee.ImageCollection(L5coll.merge(L7coll).merge(L8coll));
var collection = collection.map(function(img) { return img.updateMask(img.gt(0).and(img.lt(10000))) }) // this filter was applied according to the Landsat 1-7 product guide for validity range (page 19)
// mask clouds
collection = collection.map(function(img){  // add cloud free NDVI to all images
  var qa = img.select(['pixel_qa']);
  var mask = qa.bitwiseAnd(8).eq(0).and( // Shadow
             qa.bitwiseAnd(16).eq(0)).and( // Snow
             qa.bitwiseAnd(32).eq(0)).and( // Clouds
             img.select('B4').lt(3000))
             
             
  return img.select('B[1-5]').updateMask(mask); // previous version was: return img.addBands(img.normalizedDifference(['B4', 'B5'])).updateMask(cloudMask)
});





var addNDVI = function(image) {
  var ndvi = image.normalizedDifference(['B4', 'B3']).rename('NDVI');
  return image.addBands(ndvi).addBands(ee.Image.constant(ee.Number.parse(image.date().format("D"))).rename('DOY').float());
};

var add365 = function(image) {
  var DOI = image.select('DOY');
  var DOI365 = DOI.add(365).rename('DOI365');
  DOI365=DOI365.where(( (DOI365.gte(639))),DOI365.subtract(365));
  return image.addBands(DOI365);
};
    // 


// Function to cloud mask from the quality band of Landsat 5.
// function maskL5(image) {
//   var qa = image.select('BQA');
//   // Make a mask to exclude cloudy pixels.
//   var mask = qa.bitwiseAnd(ee.Number(2).pow(12).int()).eq(1).and(  // cirrus
//             qa.bitwiseAnd(ee.Number(2).pow(13).int()).eq(1)).or(  // cirrus
//             qa.bitwiseAnd(ee.Number(2).pow(4).int()).neq(0)).and( // cloud
//             qa.bitwiseAnd(ee.Number(2).pow(7).int()).neq(0))      // cloud
//             // Negate this.  Don't want high confidence of cloud or cirrus.
//             .not();

//   return image.updateMask(mask);
// }
// var mask = qa.bitwiseAnd(Math.pow(2, 12)).neq(1).and(  // cirrus
//           qa.bitwiseAnd(Math.pow(2, 13)).neq(1)).and( // cirrus
//           qa.bitwiseAnd(Math.pow(2, 14)).neq(1)).and( // cloud
//           qa.bitwiseAnd(Math.pow(2, 15)).neq(1));     // cloud

//Function for applying fmask to sr data

    // Bit 0: Fill
    // Bit 1: Clear
    // Bit 2: Water
    // Bit 3: Cloud Shadow
    // Bit 4: Snow
    // Bit 5: Cloud
    // Bits 6-7: Cloud Confidence
    //     0: None
    //     1: Low
    //     2: Medium
    //     3: High







// var Collection_NoCloud = collection.map(maskL5)
// print(Collection_NoCloud.limit(13))
var ndviCollection = collection.map(addNDVI)
print(ndviCollection.limit(13))

var ndviCollection365 = ndviCollection.map(add365)
print(ndviCollection365.limit(13))

var YearMaxComposite= ee.List.sequence(2004,2017,1).map(function(year){
  year = ee.Number(year)
  var start_date = ee.Date.fromYMD(year, 2, 1);
  var end_date   = ee.Date.fromYMD(year, 9, 30);
  // var start = ee.Date.fromYMD(year, 1, 1)
  var ndviCollectionMax = ndviCollection
  // .filter(ee.Filter.calendarRange(year, year.add(0),'year'))
  .filterDate(start_date,end_date)
  .qualityMosaic('NDVI')
  return ndviCollectionMax
  .set('year', year.subtract(1))
  .set('system:time_start', start_date.millis());
  // .copyProperties(ndviCollectionMax,['system:index']) //['system:index']
  })
  
  YearMaxComposite = ee.ImageCollection(YearMaxComposite).select('NDVI','DOY')
  
  print('YearMaxComposite is',YearMaxComposite)


var YearMinComposite= ee.List.sequence(2003,2016,1).map(function(year){
  year = ee.Number(year)
  // year_p1 = ee.Number(year_p1)
  var start_date = ee.Date.fromYMD(year, 10, 1);
  var end_date = start_date.advance(7, 'month')
    // var end_date   = ee.Date.fromYMD(year.add(1), 4, 30);
  // var start = ee.Date.fromYMD(year, 1, 1)
  var ndviCollectionMinNeg = ndviCollection365
  .filterDate(start_date,end_date)
  // .filter(ee.Filter.calendarRange(year, year.add(0),'year'))
  .map(function(image){
  return image.multiply(-1);
});
  
  var ndviCollectionMin = ndviCollectionMinNeg.qualityMosaic('NDVI')//.min()
  return ndviCollectionMin
  .set('year', year)
  .set('system:time_start', start_date.millis());
  // .copyProperties(ndviCollectionMin,['system:index']) //['system:index']
  })
  
  YearMinComposite = ee.ImageCollection(YearMinComposite).select('NDVI','DOI365')


YearMinComposite = YearMinComposite.map(function(image){
  var ndvi = image.select('NDVI').multiply(-1);
  var doi = image.select('DOI365').multiply(-1);
  
  return image.addBands(ndvi,['NDVI'], true).addBands(doi,['DOI365'], true);
  
  // return result.copyProperties(ee.Image(image),['system:index','system:time_start', 'year']);   
  
});

print('YearMinComposite is',YearMinComposite)


/// select day max ndvi







var results = YearMaxComposite.map(function(image) {
  return image.reduceRegions({
    collection: arable_buffer,
    reducer: ee.Reducer.median(),
    scale: 30
  }).map(function(f) {
    // Add a date property to each output feature.
    return f.set('date', image.date().format('YYYY'))
        // return f.set('year', image.date().format("YYYY"))

  }
  )
})


var results_buffer = arable.map(bufferFeature)

  Map.addLayer(results_buffer,{color:'0000FF'},'points LUCAS results');



var results_min = YearMinComposite.map(function(image) {
  return image.reduceRegions({
    collection: arable_buffer,
    reducer: ee.Reducer.median(),
    scale: 30
  }).map(function(f) {
    // Add a date property to each output feature.
    return f.set('date', image.date().format('YYYY')) //Y-M-d
        // return f.set('year', image.date().format("YYYY"))

  }
  )
})

var results_minSTD = YearMinComposite.map(function(image) {
  return image.reduceRegions({
    collection: arable_buffer,
    reducer: ee.Reducer.sampleStdDev(),
    scale: 30
  }).map(function(f) {
    // Add a date property to each output feature.
    return f.set('date', image.date().format('YYYY')) //Y-M-d
        // return f.set('year', image.date().format("YYYY"))

  }
  )
})

var results_maxSTD = YearMaxComposite.map(function(image) {
  return image.reduceRegions({
    collection: arable_buffer,
    reducer: ee.Reducer.sampleStdDev(),
    scale: 30
  }).map(function(f) {
    // Add a date property to each output feature.
    return f.set('date', image.date().format('YYYY')) //Y-M-d
        // return f.set('year', image.date().format("YYYY"))

  }
  )
})


Map.addLayer(YearMaxComposite.median(),{min:0, max:1, palette:palette1,bands:['NDVI']},'NDVImax')
Map.addLayer(YearMinComposite.median(),{min:0, max:1, palette:palette1,bands:['NDVI']},'NDVImin')

Map.addLayer(YearMaxComposite.median(),{min:120, max:220, palette:palette1,bands:['DOY']},'NDVImaxDOY')
Map.addLayer(YearMinComposite.median(),{min:0, max:50, palette:palette1,bands:['DOI365']},'NDVIminDOY')


//extract median for min and max
var max_NDVI= YearMaxComposite.median().reduceRegions({
  collection: arable_buffer,
    reducer: ee.Reducer.median(),
    scale: 30});

var min_NDVI= YearMinComposite.median().reduceRegions({
  collection: arable_buffer,
    reducer: ee.Reducer.median(),
    scale: 30});
    

// Export.table.toDrive({collection: max_NDVI, description: 'max_NDVI', 
//   selectors:  ['POINT_ID','sample_ID','GPS_LAT','GPS_LONG','NDVI','DOY'] //median
//   })


// Export.table.toDrive({collection: min_NDVI, description: 'min_NDVI', 
//   selectors:  ['POINT_ID','sample_ID','GPS_LAT','GPS_LONG','NDVI','DOY']
//   })
  


print(ee.FeatureCollection(results).flatten().limit(2))

Export.table.toDrive({collection: results.flatten(), description: 'LUCAS_Landsat_NDVI_MAX', 
  selectors: ['POINT_ID','sample_ID','GPS_LAT','GPS_LONG','NDVI','DOY','date']
  })
  
  
Export.table.toDrive({collection: results_min.flatten(), description: 'LUCAS_Landsat_NDVI_MIN', 
  selectors: ['POINT_ID','sample_ID','GPS_LAT','GPS_LONG','NDVI','DOI365','date']
  })
  
Export.table.toDrive({collection: results_minSTD.flatten(), description: 'LUCAS_Landsat_NDVI_MIN_STD', 
  selectors: ['POINT_ID','sample_ID','GPS_LAT','GPS_LONG','NDVI','DOI365','date']
  })

Export.table.toDrive({collection: results_maxSTD.flatten(), description: 'LUCAS_Landsat_NDVI_MAX_STD', 
  selectors: ['POINT_ID','sample_ID','GPS_LAT','GPS_LONG','NDVI','DOI','date']
  })



  
  Map.addLayer(arable_buffer,{color:'0000FF'},'points LUCAS buffer');

