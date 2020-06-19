console.log("2.13.2019-3")

log_transformation_methods = ['log10',"log2"]
fold_change_methods = ['median','mean']

sequence = function(from = 0, to = 10){
  var N = to-from;
  var seq = Array.apply(null, {length: N}).map(Function.call, Number);
  var result = seq.map(x=>x+from)
  return(result)
}
function save( dataBlob, filesize ){
		saveAs( dataBlob, 'D3 vis exported to PNG.png' ); // FileSaver.js function
	}

color_scale_options = ['Greys','YlGnBu','Greens','YlOrRd','Bluered','RdBu','Reds','Blues','Picnic','Rainbow','Portland','Jet','Hot','Blackbody','Earth','Electric','Viridis','Cividis']

function svgString2Image( url, width, height, format, callback ) {
	var format = format ? format : 'png';

	var imgsrc = 'data:image/svg+xml;base64,'+ btoa(unescape(url.replace("data:image/svg+xml,",""))); // Convert SVG string to data URL

	var canvas = document.createElement("canvas");
	var context = canvas.getContext("2d");

	canvas.width = width;
	canvas.height = height;

	var image = new Image();
	image.onload = function() {
		context.clearRect ( 0, 0, width, height );
		context.drawImage(image, 0, 0, width, height);

		canvas.toBlob( function(blob) {
		  bbb = blob
			var filesize = Math.round( blob.length/1024 ) + ' KB';
			//if ( callback ) callback( blob, filesize );
		});


	};

	image.src = imgsrc;
}

make_data_ready = function(ooo,id="preview_datatable"){

  var nrow = ooo.data_matrix.length
  var ncol = ooo.data_matrix[0].length
  var target_nrow = nrow
  var target_col = ncol
  var row_indexes = sequence(from=0,to=target_nrow)
  var col_indexes = sequence(from=0,to=target_col)

                 var dataSet = row_indexes.map(x=>col_indexes.map(y=>ooo.data_matrix[x][y]))
                 ddd = dataSet
                 if(typeof(preview_DataTable)!=='undefined'){
                   preview_DataTable.destroy();
                   $('#'+id).empty();
                 }
                 preview_DataTable = $('#'+id).DataTable({
                    data: dataSet,
                    columns: ooo.data_matrix[0].map(function(x, index){return {title:""}}),
                    "ordering": false,
                    "scrollX": true,
                    "lengthMenu": [[15, 25, 50, -1], [15, 25, 50, "All"]]
                 });
                 if(ctrl.parameters===undefined){
                  ctrl.parameters = {}
                 }
                 ctrl.parameters.e = ooo.e
                 ctrl.parameters.f = ooo.f
                 ctrl.parameters.p = ooo.p
                 ctrl.parameters.project_id = ooo.project_id
                 ctrl.parameters.source = ooo.path
}


make_data_ready2 = function(ooo, id="preview_datatable2"){
                 var dataSet = ooo2.data_matrix

                 if(typeof(preview_DataTable2)!=='undefined'){
                   preview_DataTable2.destroy();
                   $('#'+id).empty();
                 }
                 preview_DataTable2 = $('#'+id).DataTable({
                    data: dataSet,
                    columns: ooo2.data_matrix[0].map(function(x, index){return {title:""}}),
                    "ordering": false,
                    "scrollX": true,
                    "lengthMenu": [[15, 25, 50, -1], [15, 25, 50, "All"]]
                 });
                 if(ctrl.parameters===undefined){
                  ctrl.parameters = {}
                 }
                 ctrl.parameters.e2 = ooo2.e
                 ctrl.parameters.f2 = ooo2.f
                 ctrl.parameters.p2 = ooo2.p
}

function filterOutliers(someArray) {

    // Copy the values, rather than operating on references to existing values
    var values = someArray.concat();

    // Then sort
    values.sort( function(a, b) {
            return a - b;
         });

    /* Then find a generous IQR. This is generous because if (values.length / 4)
     * is not an int, then really you should average the two elements on either
     * side to find q1.
     */
    var q1 = values[Math.floor((values.length / 4))];
    // Likewise for q3.
    var q3 = values[Math.ceil((values.length * (3 / 4)))];
    var iqr = q3 - q1;

    // Then find min and max values
    var maxValue = q3 + iqr*1.5;
    var minValue = q1 - iqr*1.5;

    // Then filter anything beyond or beneath these values.
    var filteredValues = values.filter(function(x) {
        return (x <= maxValue) && (x >= minValue);
    });

    // Then return
    return filteredValues;
}
function shuffle(o) {
    for(var j, x, i = o.length; i; j = parseInt(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
};

function test(x){
  return(jStat.mean([x,x*x,x*x*x]))
}
    function getRandom(arr, n) {
        var result = new Array(n),
            len = arr.length,
            taken = new Array(len);
        if (n > len)
            throw new RangeError("getRandom: more elements taken than available");
        while (n--) {
            var x = Math.floor(Math.random() * len);
            result[n] = arr[x in taken ? taken[x] : x];
            taken[x] = --len in taken ? taken[len] : len;
        }
        return result;
    }
    function indexOfSmallest(a) {
     var lowest = 0;
     for (var i = 1; i < a.length; i++) {
      if (a[i] < a[lowest]) lowest = i;
     }
     return lowest;
    }
loess_wrapper_extrapolate = function(x, y, span_vals = _.range(0.01, 1, 0.01), folds=5, iterations=iter, accuracy=acy,ratio=0.8){
       var mean_abs_error = Array.apply(null, Array(span_vals.length)).map(Number.prototype.valueOf,0);
       for(var i=0; i<mean_abs_error.length;i++){

        var loess2 = loess();
        loess2.bandwidth(span_vals[i])
        //.robustnessIterations(iterations).accuracy(accuracy);
        var error = []
        for(var k=0;k<folds;k++){
          var cv_index = Array.apply(null, {length: x.length}).map(Number.call, Number)
          var cv_weights = Array.apply(null, Array(cv_index.length)).map(Number.prototype.valueOf,1);

          var cv_index_train = getRandom(cv_index , Math.floor(ratio * cv_index.length))
          var cv_index_test = cv_index.filter( function( el ) { return cv_index_train.indexOf( el ) < 0;});

          var cv_weights = cv_weights.map((y,idx)=>cv_index_train.indexOf(idx)==-1? 0 : 1)

        try {
                var yValuesSmoothed = loess2(x, y, cv_weights);
                var y_predicted = cv_index_test.map(idx => yValuesSmoothed[idx])
                if(y_predicted.includes(NaN)){
                  k--;
                }else{
                  error[k] = jStat.mean(jStat.abs(cv_index_test.map(idx=>y[idx]).map((v,idx) => v-y_predicted[idx])).map(x=>isNaN(x)?null:x))
                }
            }
          catch(err) {
            error[k] = null
            }

        }
        if(error.filter(unique).length == 1){
          mean_abs_error[i] = NaN
        }else{
          mean_abs_error[i] = jStat.mean(error)
        }
       }
       var best_span = span_vals[indexOfSmallest(mean_abs_error.map(x=>isNaN(x)?Infinity:x))]
       return(best_span)
  }
function science_stats_loessUpdateBandwidthInterval(
  xval, weights, i, bandwidthInterval) {

  var left = bandwidthInterval[0],
      right = bandwidthInterval[1];

  // The right edge should be adjusted if the next point to the right
  // is closer to xval[i] than the leftmost point of the current interval
  var nextRight = science_stats_loessNextNonzero(weights, right);
  if ((nextRight < xval.length) && (xval[nextRight] - xval[i]) < (xval[i] - xval[left])) {
    var nextLeft = science_stats_loessNextNonzero(weights, left);
    bandwidthInterval[0] = nextLeft;
    bandwidthInterval[1] = nextRight;
  }
}

function science_stats_loessNextNonzero(weights, i) {
  var j = i + 1;
  while (j < weights.length && weights[j] === 0) j++;
  return j;
}

/*bandwidthInPoints = 129
xval = Array.apply(null, {length: ooo.e[0].length}).map(Number.call, Number)
weights = Array.apply(null, Array(ooo.e[0].length)).map(Number.prototype.valueOf,1);
n = xval.length
accuracy = 1e-12
robustnessWeights = weights
*/



  /*var bandwidthInterval = [0, bandwidthInPoints - 1];
	var edge_seq = [],
	bandwidthInterval_seq=[],
	denom_seq = [];
	i = -1; while (++i < n) {
		x = xval[i];

		// Find out the interval of source points on which
		// a regression is to be made.
		if (i > 0) {
		  science_stats_loessUpdateBandwidthInterval(xval, weights, i, bandwidthInterval);
		}

		var ileft = bandwidthInterval[0],
			iright = bandwidthInterval[1];
		edge_seq.push((xval[i] - xval[ileft]) > (xval[iright] - xval[i]) ? ileft : iright);
		bandwidthInterval_seq.push([ileft, iright])


    denom = Math.abs(1 / (xval[edge_seq[i]] - x));
		denom_seq.push(denom)
    }*/



/*var gpu = new GPU();



var LOESS_GPU = function(bandwidth=0.3, robustnessIters = 2, accuracy = 1e-12, xval, yval, weights){
  var n = xval.length,i;
  var bandwidthInPoints = Math.floor(bandwidth * n)
  if (bandwidthInPoints < 2) throw {error: "Bandwidth too small."};
  var robustnessWeights = [];
  i = -1; while (++i < n) {
    robustnessWeights[i] = 1;
  }
  var bandwidthInterval = [0, bandwidthInPoints - 1];
	var edge_seq = [],
	bandwidthInterval_seq=[],
	denom_seq = [];
	i = -1; while (++i < n) {
		var x = xval[i];
		// Find out the interval of source points on which
		// a regression is to be made.
		if (i > 0) {
		  science_stats_loessUpdateBandwidthInterval(xval, weights, i, bandwidthInterval);
		}
		var ileft = bandwidthInterval[0],
			iright = bandwidthInterval[1];
		edge_seq.push((xval[i] - xval[ileft]) > (xval[iright] - xval[i]) ? ileft : iright);
		bandwidthInterval_seq.push([ileft, iright])
    denom = Math.abs(1 / (xval[edge_seq[i]] - x));
		denom_seq.push(denom)
    }


  var iter = -1;
  while (++iter <= robustnessIters) {


    var LOESS_GPU_core = gpu.createKernel(function(xval,yval, weights,bandwidthInterval_seq, edge_seq, robustnessWeights, denom_seq) {

	var i = this.thread.x
	var j = this.thread.y

	var x = xval[i]
	var ileft = bandwidthInterval_seq[i][0]
	var iright = bandwidthInterval_seq[i][1]

	var sumWeights = 0,
		sumX = 0,
		sumXSquared = 0,
		sumY = 0,
		sumXY = 0,
		denom = denom_seq[i]

	for (var k = ileft; k <= iright; ++k) {
	  var xk   = xval[k];
		  var yk   = yval[k];
		  var dist = (x - xk) * Math.sign(i-k);
		  var temp_x = dist * denom;
		  temp_x =  1 - temp_x * temp_x * temp_x
		  temp_x = temp_x * temp_x * temp_x;
		  var w  = temp_x * robustnessWeights[k] * weights[k];

		  var xkw  = xk * w;

	  sumWeights += w;
	  sumX += xkw;
	  sumXSquared += xk * xkw;
	  sumY += yk * w;
	  sumXY += yk * xkw;
	}


	var meanX = sumX / sumWeights,
            meanY = sumY / sumWeights,
            meanXY = sumXY / sumWeights,
            meanXSquared = sumXSquared / sumWeights;
	var temp_accuracy = Math.sqrt(Math.abs(meanXSquared - meanX * meanX))

	var beta = (Math.sign(this.constants.accuracy - temp_accuracy)-1) * (-0.5*((meanXY - meanX * meanY) / (meanXSquared - meanX * meanX)))

	var alpha = meanY - beta * meanX;

	var res = beta * x + alpha;
	var residuals = Math.abs(yval[i] - res);

	var result = -1;

	if(j === 0){
	  result = res
	}
	if(j === 1){
	  result = residuals
	}

	//return(xval[edge_seq[i]])
	return(result)

},{
  constants: { weights_length: xval.length ,accuracy : 1e-12},
  output: [xval.length,2]
})
    o = LOESS_GPU_core(xval,yval,weights,bandwidthInterval_seq, edge_seq, robustnessWeights, denom_seq)
    if (iter === robustnessIters) {
      break;
    }
    var medianResidual = jStat.median(o[1]);
    if (Math.abs(medianResidual) < accuracy){
      break;
    }
    var arg,w;
    i = -1;
    while (++i < n) {
      arg = o[1][i] / (6 * medianResidual);
      robustnessWeights[i] = (arg >= 1) ? 0 : ((w = 1 - arg * arg) * w);
    }

  }
  return(o[0])
}

loess_wrapper_extrapolate_GPU = function(x, y, span_vals = _.range(0.01, 1, 0.01), folds=5, iterations, accuracy,ratio=0.8){
       var mean_abs_error = Array.apply(null, Array(span_vals.length)).map(Number.prototype.valueOf,0);
       for(var i=0; i<mean_abs_error.length;i++){

        //var loess2 = science.stats.loess();
        //loess2.bandwidth(span_vals[i])
        //.robustnessIterations(iterations).accuracy(accuracy);
        var error = []
        var best_span = 0;
        for(var k=0;k<folds;k++){
          var cv_index = Array.apply(null, {length: x.length}).map(Number.call, Number)
          var cv_weights = Array.apply(null, Array(cv_index.length)).map(Number.prototype.valueOf,1);

          var cv_index_train = getRandom(cv_index , Math.floor(ratio * cv_index.length))
          var cv_index_test = cv_index.filter( function( el ) { return cv_index_train.indexOf( el ) < 0;});

          var cv_weights = cv_weights.map((y,idx)=>cv_index_train.indexOf(idx)==-1? 0 : 1)

        try {
                var yValuesSmoothed = LOESS_GPU(span_vals[i], 2, 1e-12, x, y, cv_weights)
                //loess2(x, y, cv_weights);
                var y_predicted = cv_index_test.map(idx => yValuesSmoothed[idx])
                if(y_predicted.includes(NaN)){
                  k--;
                }else{
                  error[k] = jStat.mean(jStat.abs(cv_index_test.map(idx=>y[idx]).map((v,idx) => v-y_predicted[idx])).map(x=>isNaN(x)?null:x))
                }
            }
          catch(err) {

            best_span = err
            error[k] = null
            }

        }
       }
        if(error.filter(unique).length == 1){
          mean_abs_error[i] = NaN
        }else{
          mean_abs_error[i] = jStat.mean(error)
        }

       var best_span = span_vals[indexOfSmallest(mean_abs_error.map(x=>isNaN(x)?Infinity:x))]
       //return(best_span)

  }*/

serrf_wrapper_extrapolate = function(x, y, span_vals = _.range(0.01, 1, 0.01), folds=5, iterations=iter, accuracy=acy,ratio=0.8){
       var mean_abs_error = Array.apply(null, Array(span_vals.length)).map(Number.prototype.valueOf,0);
       for(var i=0; i<mean_abs_error.length;i++){
        var loess2 = loess();
        loess2.bandwidth(span_vals[i])
        //.robustnessIterations(iterations).accuracy(accuracy);
        var error = []
        for(var k=0;k<folds;k++){
          var cv_index = Array.apply(null, {length: x.length}).map(Number.call, Number)
          var cv_weights = Array.apply(null, Array(cv_index.length)).map(Number.prototype.valueOf,1);
          var cv_index_train = getRandom(cv_index , Math.floor(ratio * cv_index.length))
          var cv_index_test = cv_index.filter( function( el ) { return cv_index_train.indexOf( el ) < 0;});

          var cv_weights = cv_weights.map((y,idx)=>cv_index_train.indexOf(idx)==-1? 0 : 1)

        try {
                var yValuesSmoothed = loess2(x, y, cv_weights);
                var y_predicted = cv_index_test.map(idx => yValuesSmoothed[idx])
                if(y_predicted.includes(NaN)){
                  k--;
                }else{
                  error[k] = jStat.mean(jStat.abs(cv_index_test.map(idx=>y[idx]).map((v,idx) => v-y_predicted[idx])).map(x=>isNaN(x)?null:x))
                }
            }
          catch(err) {
            error[k] = null
            }

        }
        if(error.filter(unique).length == 1){
          mean_abs_error[i] = NaN
        }else{
          mean_abs_error[i] = jStat.mean(error)
        }
       }
       var best_span = span_vals[indexOfSmallest(mean_abs_error.map(x=>isNaN(x)?Infinity:x))]
       return(best_span)
  }

function sum(xs) {
  return xs.reduce((total, x) => total + x, 0)
}

function mean(xs) {
  return sum(xs) / xs.length
}

function zip(xs, ys) {
  return range(xs.length)
    .reduce((arr, i) => [...arr, [xs[i], ys[i]]], [])
}

function range(start, end) {
  if (end == null) {
    end = start
    start = 0
  }

  return new Array(end - start).fill().map((_, i) => i + start)
}

function stdev(xs) {
  const xhat = mean(xs)
  const squareDistances = xs.map(x => Math.pow(x - xhat, 2))
  return Math.sqrt(sum(squareDistances) / (xs.length - 1))
}

function covariance(xs, ys) {
  const xhat = mean(xs)
  const yhat = mean(ys)

  const total = sum(zip(xs, ys).map(([x, y]) => (x - xhat) * (y - yhat)))

  return total / (xs.length - 1)
}



function delta(aij, res, i, j) {
	for (var k=0, sum=aij; k<j; ++k) if (res[i][k]) sum -= res[i][k] * res[j][k]
	return sum
}
function cholesky(matrix) {
	var len = matrix.length,
			res = Array(len)
	if (matrix.length !== matrix[len-1].length) throw Error('Input matrix must be square or lower triangle')

	res[0] = [Math.sqrt( matrix[0][0] )]

	for (var i = 1; i<len; ++i) {
		res[i] = Array(i+1); // lower triangle
		for (var j = 0; j < i; ++j) {
			res[i][j] = delta(matrix[i][j], res, i, j) / res[j][j]
		}
		res[i][i] = Math.sqrt(delta(matrix[i][i], res, i, i))
	}
	return res
}



ellipse = function(x,y, level = 0.95){
  var dfn = 2
  var segments = 51
  var dfd = x.length-1
  if(dfd<2){
    return({x:null,y:null})
  }else{

    var shape = [[covariance(x,x), covariance(x,y)],[covariance(y,x), covariance(y,y)]]
    var center = [mean(x), mean(y)]
    var chol_decomp = cholesky(shape)
    chol_decomp = [[chol_decomp[0][0], chol_decomp[1][0]],[0,chol_decomp[1][1]]]

    var radius =  Math.sqrt(dfn * jStat.centralF.inv(level, dfn, dfd))

    var angles = Array.apply(null, {length: segments+1}).map(Number.call, Number).map(x => x*2*(Math.PI/segments))
    var unit_circle = angles.map(x=>[Math.cos(x),Math.sin(x)])
    var b = jStat(jStat.transpose(math.multiply(unit_circle, chol_decomp))).multiply( radius )

    var ellipse = []
    for(var i=0; i<b.length;i++){
      ellipse.push(b[i].map(x=>center[i]+x))
    }
    return(ellipse)
  }
}




revalue = function(array, old_val, new_val){
  var result = [];

  for(var j=0; j<array.length;j++){

    for(var i=0; i<old_val.length;i++){

      if(array[j] === old_val[i]){
        result.push( new_val[i])
      }

    }

  }

  return(result)

}
function groupData(labels, values) {
  return labels.reduce(function(hash, lab, i) { // for each label lab
    if(hash[lab])                               // if there is an array for the values of this label
      hash[lab].push(values[i]);                // push the according value into that array
    else                                        // if there isn't an array for it
      hash[lab] = [ values[i] ];                // then create one that initially contains the according value
    return hash;
  }, {});
}



// color_by = unpack(ooo.p,"species")
// shape_by = unpack(ooo.p,"treatment")
// color_option = ["red", "blue"]
// shape_option = ["circle", "square", "diamond", "cross"]
// scores = jStat.transpose(oo.scores)
// variance = oo.variance
// color_levels = ["pumpkin", "tomatillo"]
// shape_levels = ["MeOH-frz", "ACN.IPA-frz", "MeOH.CHCl3-frz", "MeOH.CHCl3-lyph"]
// labels = unpack(ooo.p, 'label')
// scatter_size = 6
//

pair_score_plot = function(scores,variance, color_by, shape_by, color_option, shape_option, color_levels, shape_levels, labels, scatter_size){


    if(shape_by.filter(unique)[0]===undefined){
      var shape_by = Array(shape_by.length).fill("")
      var shape_levels = [""]
    }
    if(color_by.filter(unique)[0]===undefined){
      var color_by = Array(color_by.length).fill("")
      var color_levels = [""]
    }

  var num_PC = jStat.min([5,jStat.transpose(oo.scores).length])


  var color = revalue(color_by, color_levels,color_option)
  var shape = revalue(shape_by, shape_levels,shape_option)

  var data = [];
  var group = color.map((x,i)=> x+"+"+shape[i])

  var grouped_scores = scores.map(x=>groupData(group,x))
  var grouped_labels = groupData(group,labels)
  var grouped_color = groupData(group,color)
  var grouped_shape = groupData(group,shape)

  // scatters
  for(var groupi=0;groupi<Object.keys(grouped_scores[0]).length;groupi++){
    var group_type = Object.keys(grouped_scores[0])[groupi]
    for(var yi=0;yi<num_PC;yi++){
      for(var xi=0;xi<num_PC;xi++){
        if(xi!==yi){
          data.push({
            x:grouped_scores[xi][group_type],
            y:grouped_scores[yi][group_type],
            text:grouped_labels[group_type],
            type:"scatter",
            mode:"markers",
            marker:{
              autocolorscale:false,
              color:group_type.split("+")[0],
              opacity:1,
              size:scatter_size,
              symbol:group_type.split("+")[1]
            },
            hoveron:"points",
            name:color_levels[color_option.indexOf(grouped_color[group_type][0])] + " " + shape_levels[shape_option.indexOf(grouped_shape[group_type][0])],
            legendgroup:color_levels[color_option.indexOf(grouped_color[group_type][0])] + " " + shape_levels[shape_option.indexOf(grouped_shape[group_type][0])],
            showlegend:false,
            xaxis:"x"+(xi==0?"":(xi+1)),
            yaxis:"y"+(yi==0?"":(yi+1)),
            hoverinfo:"text"
          })
        }else{
          data.push({
            x:grouped_scores[xi][group_type],
            y:grouped_scores[yi][group_type],
            type:"scatter",
            mode:"markers",
            marker:{
              autocolorscale:false,
              color:group_type.split("+")[0],
              opacity:1,
              size:scatter_size,
              symbol:group_type.split("+")[1]
            },
            text:"PC"+xi+": "+(variance[xi]*100).toFixed(2)+"%",
            xaxis:"x"+(xi==0?"":(xi+1)),
            yaxis:"y"+(yi==0?"":(yi+1)),
            showlegend:false,
            hoverinfo:"text"
          })
        }
      }
    }
  }

 var layout = {
   /*margin:{
     t:40.57658,
     r:18.99543,
     b:42.84142,
     l:48.94977
   },*/
   plot_bgcolor:"rgba(255,255,255,1)",
   paper_bgcolor:"rgba(255,255,255,1)",
   font:{
     color:"rgba(0,0,0,1)",
     family:"Dorid Sans",
     size:18
   },
   showlegend:true,
   legend:{
     bgcolor:"rgba(255,255,255,1)",
     bordercolor:"transparent",
     borderwidth:1.88,
     font:{
       color:"rgba(0,0,0,1)",
       family:"Dorid Sans",
       size:15
     },
     y:0.8148116
   },
   hovermode:"closest",
   barmode:"relative"
 }

 // x/y axis
 var range=[];
 var x_domains = [[0.0000000,0.190775], [0.2092247,0.3907753], [0.4092247 ,0.5907753], [0.6092247 ,0.7907753], [0.8092247 ,1.0000000]]
 var y_domains = [[0.8167184,1.0000000], [0.6167184, 0.7832816], [0.4167184 ,0.5832816], [0.2167184 ,0.3832816], [0.0000000,0.1832816]]
 for(var pci=0;pci<num_PC;pci++){
   var bounds = nice_bounds(jStat.min(scores[pci]), jStat.max(scores[pci]))
   layout["xaxis"+ (pci==0? "":(pci+1))]={
     type:"linear",
     autorange:true,
     //range:[bounds.min,bounds.max],
     //tickmode:"array",
     //ticktext:_.range(bounds.min, bounds.max, bounds.steps),
     //tickvals:_.range(bounds.min, bounds.max, bounds.steps),
     //categoryorder:"array",
     //categoryarray:_.range(bounds.min, bounds.max, bounds.steps),
     //nticks:null,
     ticks:"outside",
     tickcolor:"rgba(0,0,0,1)",
     ticklen:3.6,
     tickwidth:0.66,
     showticklabels:true,
     tickfont:{
       color:"rgba(0,0,0,1)",
       family:"Dorid Sans",
       size:15
     },
     tickangle:0,
     showline:false,
     linecolor:null,
     linewidth:0,
     showgrid:true,
     domain:x_domains[pci],
     gridcolor:null,
     gridwidth:0,
     zeroline:false,
     anchor:"y5",
     title:""
   }

   layout["yaxis"+(pci==0? "":(pci+1))]={
     type:"linear",
     autorange:true,
     //range:[jStat.min([bounds.min,jStat.min(data[unpack(data, "yaxis").indexOf("y"+ (pci==0? "":(pci+1)))].y)]), jStat.max([bounds.max,jStat.max(data[unpack(data, "yaxis").indexOf("y"+ (pci==0? "":(pci+1)))].y)])],
     //range:[bounds.min,bounds.max],
     //tickmode:"array",
     //ticktext:_.range(bounds.min, bounds.max, bounds.steps),
     //tickvals:_.range(bounds.min, bounds.max, bounds.steps),
     //categoryorder:"array",
     //categoryarray:_.range(bounds.min, bounds.max, bounds.steps),
     //nticks:null,
     ticks:"outside",
     tickcolor:"rgba(0,0,0,1)",
     ticklen:3.6,
     tickwidth:0.66,
     showticklabels:true,
     tickfont:{
       color:"rgba(0,0,0,1)",
       family:"Dorid Sans",
       size:15
     },
     tickangle:0,
     showline:false,
     linecolor:null,
     linewidth:0,
     showgrid:true,
     domain:y_domains[pci],
     gridcolor:null,
     gridwidth:0,
     zeroline:false,
     anchor:"x",
     title:""
   }
 }

  // borders
  layout.shapes = []
   for(var border_x=0;border_x<num_PC;border_x++){
     for(var border_y=0;border_y<num_PC;border_y++){
       layout.shapes.push({
         type:"rect",
         fillcolor:"transparent",
         line:{
           color:"rgba(0,0,0,1)",
           width:0.66,
           linetype:"solid"
         },
         yref:"paper",
         xref:"paper",
         x0:x_domains[border_x][0],
         x1:x_domains[border_x][1],
         y0:y_domains[border_y][0],
         y1:y_domains[border_y][1]
       })
     }
   }
  // axis text
  layout.annotations = []
   for(var border_x=0;border_x<num_PC;border_x++){
     layout.annotations.push({
       text:"PC "+(border_x+1) +": ("+(variance[border_x]*100).toFixed(2)+"%)",
       x:jStat.mean(x_domains[border_x]),
       y:1,
       showarrow:false,
       ax:0,
       ay:0,
       font:{
         color:"rgba(0,0,0,1)",
         family:"Dorid Sans",
         size:15
       },
       xref:"paper",
       yref:"paper",
       textangle:0,
       xanchor:"center",
       yanchor:"bottom"
     })
   }
   for(var border_y=0;border_y<num_PC;border_y++){
     layout.annotations.push({
       text:"PC "+((5-border_y)),
       x:1,
       y:jStat.mean(x_domains[border_y]),
       showarrow:false,
       ax:0,
       ay:0,
       font:{
         color:"rgba(0,0,0,1)",
         family:"Dorid Sans",
         size:15
       },
       xref:"paper",
       yref:"paper",
       textangle:90,
       xanchor:"left",
       yanchor:"middle"
     })
   }
  // showlegend, hovermode, barmode
  layout.showlegend = true
  layout.hovermode = "closest"
  layout.barmode = "relative"
  layout.autosize = false
  layout.height = 750
  layout.width = 750

//Plotly.newPlot('score_plot', data, layout);
  return({data:data, layout:layout})
}

// color_by = unpack(ooo.p,"species")
// shape_by = unpack(ooo.p,"treatment")
// color_option = ["rgba(0,175,187,1)", "rgba(252,78,7,1)"]
// shape_option = ["circle", "square", "diamond", "cross"]
// scores = jStat.transpose(oo.scores)
// x = scores[0]
// y = scores[1]
// variance = oo.variance
// color_levels = ["pumpkin", "tomatillo"]
// shape_levels = ["MeOH-frz", "ACN.IPA-frz", "MeOH.CHCl3-frz", "MeOH.CHCl3-lyph"]
// labels = unpack(ooo.p, 'label')
// scatter_size = 6
// xlab = "PC 1 (1%)"
// ylab = "PC 2 (1%)"
// add_center = true
// ellipse_shape = true
// ellipse_color = true
color_palette = {}
color_palette.mpn65 = ["ff0029", "377eb8", "66a61e", "984ea3", "00d2d5", "ff7f00", "af8d00", "7f80cd", "b3e900", "c42e60", "a65628", "f781bf", "8dd3c7", "bebada", "fb8072", "80b1d3", "fdb462", "fccde5", "bc80bd", "ffed6f", "c4eaff", "cf8c00", "1b9e77", "d95f02", "e7298a", "e6ab02", "a6761d", "0097ff", "00d067", "000000", "252525", "525252", "737373", "969696", "bdbdbd", "f43600", "4ba93b", "5779bb", "927acc", "97ee3f", "bf3947", "9f5b00", "f48758", "8caed6", "f2b94f", "eff26e", "e43872", "d9b100", "9d7a00", "698cff", "d9d9d9", "00d27e", "d06800", "009f82", "c49200", "cbe8ff", "fecddf", "c27eb6", "8cd2ce", "c4b8d9", "f883b0", "a49100", "f48800", "27d0df", "a04a9b"].map(x=>hexToRgb(x,1))

shape_palette = {}
shape_palette.ggplot2 = ["circle", "triangle-up", "square", "cross-thin", "square-x-open", "asterisk-open"]

shape_by_options = ["circle", "triangle-up", "square", "cross-thin", "square-x-open", "asterisk-open"]

transparent_rgba = function(rgba,alpha=0.1){
  var splits = rgba.split(",")
  if(splits.length==3){
    var splits = rgba.split("(").map(x=>x.split(")"))
    splits = splits[1]

    return("rgba("+[splits[0],alpha+")"].join(","))
  }else{
    return([splits[0],splits[1],splits[2],alpha+")"].join(","))
  }

}

score_plot = function(x,y,xlab, ylab, variance, color_by, shape_by, color_option, shape_option, color_levels, shape_levels, labels, scatter_size, add_center, ellipse_color, ellipse_shape){

  var ellipse_color = ellipse_color == true
  var ellipse_shape = ellipse_shape == true
  var add_center = add_center == true


    if(ellipse_color==false){
      var ellipse_color_by = Array(color_by.length).fill("")
      var ellipse_color_levels = [""]
    }else{
      var ellipse_color_by = color_by
      var ellipse_color_levels = color_levels
    }
    if(ellipse_shape==false){
      var ellipse_shape_by = Array(shape_by.length).fill("")
      var ellipse_shape_levels = [""]
    }else{
      var ellipse_shape_by = shape_by
      var ellipse_shape_levels = shape_levels
    }

    if(shape_by.filter(unique)[0]===undefined){
      var shape_by = Array(shape_by.length).fill("")
      var shape_levels = [""]
    }
    if(color_by.filter(unique)[0]===undefined){
      var color_by = Array(color_by.length).fill("")
      var color_levels = [""]
    }



    var color = revalue(color_by, color_levels,color_option)
    var shape = revalue(shape_by, shape_levels,shape_option)

    var ellipse_colors = revalue(ellipse_color_by, ellipse_color_levels,color_option)
    var ellipse_shapes = revalue(ellipse_shape_by, ellipse_shape_levels,shape_option)

    var data = [];
    var group = color.map((x,i)=> x+"+"+shape[i])

    var grouped_scores_x = groupData(group,x)
    var grouped_scores_y = groupData(group,y)
    var grouped_labels = groupData(group,labels)
    var grouped_color = groupData(group,color)
    var grouped_shape = groupData(group,shape)


    // deterine if ellipse is needed
    var ellipse_group = ellipse_colors.map((x,i)=> x+"+"+ellipse_shapes[i])
    if(ellipse_color && ellipse_shape){
      ellipse_group_x =  groupData(ellipse_group,x)
      ellipse_group_y =  groupData(ellipse_group,y)
    }else if(ellipse_color){
      //var ellipse_group = color.map((x,i)=> x)
      ellipse_group_x =  groupData(ellipse_group,x)
      ellipse_group_y =  groupData(ellipse_group,y)
    }else if(ellipse_shape){
      //var ellipse_group = shape.map((x,i)=> x)
      ellipse_group_x =  groupData(ellipse_group,x)
      ellipse_group_y =  groupData(ellipse_group,y)
    }else{
      //var ellipse_group = null
      ellipse_group_x = {}
      ellipse_group_y = {}
    }

    // these remeber the minimum and maximum value of each ellipse so that the range of xaxis and y axsis can be moderate.
    min_x_ellipse = 0
    max_x_ellipse = 0
    min_y_ellipse = 0
    max_y_ellipse = 0


    // add ellipse
    for(var ellipse_groupi=0;ellipse_groupi<Object.keys(ellipse_group_x).length;ellipse_groupi++){
       var ellipse_group_type = Object.keys(ellipse_group_x)[ellipse_groupi]
       /*console.log(ellipse_groupi)
       aaa = ellipse_group_x[ellipse_group_type]
       bbb = ellipse_group_y[ellipse_group_type]*/
       var ellipse_dta = ellipse(ellipse_group_x[ellipse_group_type],ellipse_group_y[ellipse_group_type]);
       eeeeee = ellipse_dta
       if(eeeeee.x !== null){
        min_x_ellipse = jStat.min([min_x_ellipse,jStat.min(ellipse_dta[0])])
        max_x_ellipse = jStat.max([max_x_ellipse,jStat.max(ellipse_dta[0])])
        min_y_ellipse = jStat.min([min_y_ellipse,jStat.min(ellipse_dta[0])])
        max_y_ellipse = jStat.max([min_y_ellipse,jStat.max(ellipse_dta[0])])


       data.push({
         x:ellipse_dta[0],
         y:ellipse_dta[1],
         text:null,
         type:"scatter",
         mode:"lines",
         line:{
           width:1.889764,
           color:transparent_rgba(ellipse_group_type.split("+")[0],0.1),
           dash:"solid"
         },
         fill:"toself",
         fillcolor:transparent_rgba(ellipse_group_type.split("+")[0],0.1),
         name:color_levels[color_option.indexOf(grouped_color[ellipse_group_type][0])] + " " + shape_levels[shape_option.indexOf(grouped_shape[ellipse_group_type][0])],
         legendgroup:"",
         showlegend:false,
         xaxis:"x",
         yaxis:"y",
         hoverinfo:"skip"
       })
       }
       
     }

    // data
    for(var groupi=0;groupi<Object.keys(grouped_scores_x).length;groupi++){
      var group_type = Object.keys(grouped_scores_x)[groupi]

      if(shape_levels[shape_option.indexOf(grouped_shape[group_type][0])]==''){
        var name = color_levels[color_option.indexOf(grouped_color[group_type][0])]
      }else if(color_levels[color_option.indexOf(grouped_color[group_type][0])]==''){
        var name = shape_levels[shape_option.indexOf(grouped_shape[group_type][0])]
      }else{
        var name = color_levels[color_option.indexOf(grouped_color[group_type][0])] + "<br />" + shape_levels[shape_option.indexOf(grouped_shape[group_type][0])]
      }
          data.push({
            x:grouped_scores_x[group_type],
            y:grouped_scores_y[group_type],
            text:grouped_labels[group_type],
            type:"scatter",
            mode:"markers",
            marker:{
              autocolorscale:false,
              color:group_type.split("+")[0],
              opacity:1,
              size:scatter_size,
              symbol:group_type.split("+")[1],
              line:{
                width:1,
                color:"rgba(0,0,0,1)"
              }
            },
            hoveron:"points",
            name:name,
            legendgroup:color_levels[color_option.indexOf(grouped_color[group_type][0])] + " " + shape_levels[shape_option.indexOf(grouped_shape[group_type][0])],
            showlegend:name!=="",
            xaxis:"x",
            yaxis:"y",
            hoverinfo:"text"
         })
         if(add_center){
          data.push({
            x:[jStat.median(grouped_scores_x[group_type])],
            y:[jStat.median(grouped_scores_y[group_type])],
            text:"center of "+color_levels[color_option.indexOf(grouped_color[group_type][0])] + " " + shape_levels[shape_option.indexOf(grouped_shape[group_type][0])],
            type:"scatter",
            mode:"markers",
            marker:{
              autocolorscale:false,
              color:group_type.split("+")[0],
              opacity:1,
              size:scatter_size*2,
              symbol:group_type.split("+")[1],
              line:{
                width:1,
                color:"rgba(0,0,0,1)"
              }
            },
            hoveron:"points",
            name:name,
            legendgroup:color_levels[color_option.indexOf(grouped_color[group_type][0])] + " " + shape_levels[shape_option.indexOf(grouped_shape[group_type][0])],
            showlegend:false,
            xaxis:"x",
            yaxis:"y",
            hoverinfo:"text"
         })
         }
    }

    // x axis bound
    var bounds_x = nice_bounds(jStat.min([min_x_ellipse,jStat.min(x)]), jStat.max([max_x_ellipse,jStat.max(x)]))
    var bounds_y = nice_bounds(jStat.min([min_y_ellipse,jStat.min(y)]), jStat.max([max_y_ellipse,jStat.max(y)]))

    // x/y zero line
    data.push({
      x:[bounds_x.min,bounds_x.max],
      y:[0,0],
      type:"scatter",
      line:{
        width:1,
        color:"rgba(0,0,0,1)",
        dash:"dash"
      },
      hoveron:"points",
      showlegend:false,
      xaxis:"x",
      yaxis:"y",
      hoverinfo:"skip",
      name:""
    })
    data.push({
      x:[0,0],
      y:[bounds_y.min,bounds_y.max],
      type:"scatter",
      line:{
        width:1,
        color:"rgba(0,0,0,1)",
        dash:"dash"
      },
      hoveron:"points",
      showlegend:false,
      xaxis:"x",
      yaxis:"y",
      hoverinfo:"skip",
      name:""
    })

    // layout
    layout = {
      /*margin:{
       t:40.57658,
       r:18.99543,
       b:42.84142,
       l:68.94977
      },*/
      font:{
        color:"rgba(0,0,0,1)",
        family:"Dorid Sans",
        size:15
      },
      title:"<b>Score Plot</b>",
      titlefont:{
        color:"rgba(0,0,0,1)",
        family:"Dorid Sans",
        size:20
      },
      plot_bgcolor:"rgba(255,255,255,1)",
      paper_bgcolor:"rgba(255,255,255,1)",
      xaxis:{
       type:"linear",
       autorange:true,
       //range:[bounds_x.min,bounds_x.max],
       tickmode:"array",
       //ticktext:_.range(bounds_x.min, bounds_x.max, bounds_x.steps).map(x => x<0? Number(x.toFixed(2)).toExponential():x.toFixed(2)),
       //tickvals:_.range(bounds_x.min, bounds_x.max, bounds_x.steps),
       //categoryorder:"array",
       //categoryarray:_.range(bounds_x.min, bounds_x.max, bounds_x.steps),
       //nticks:null,
       ticks:"outside",
       tickcolor:"rgba(0,0,0,1)",
       ticklen:3.6,
       tickwidth:0.66,
       showticklabels:true,
       tickfont:{
         color:"rgba(0,0,0,1)",
         family:"Dorid Sans",
         size:15
       },
       tickangle:0,
       showline:false,
       linecolor:null,
       linewidth:0,
       showgrid:true,
       domain:[0,1],
       gridcolor:null,
       gridwidth:0,
       zeroline:false,
       anchor:"y",
       title:xlab,
       titlefont:{
         color:"rgba(0,0,0,1)",
         family:"Dorid Sans",
         size:18
       }
     },
     yaxis:{
       type:"linear",
       autorange:true,
       //range:[bounds_y.min,bounds_y.max],
       tickmode:"array",
       //ticktext:_.range(bounds_y.min, bounds_y.max,bounds_y.steps).map(x => x<0? Number(x.toFixed(2)).toExponential():x.toFixed(2)),
       //tickvals:_.range(bounds_y.min, bounds_y.max,bounds_y.steps),
       //categoryorder:"array",
       //categoryarray:_.range(bounds_y.min, bounds_y.max,bounds_y.steps),
       //nticks:null,
       ticks:"outside",
       tickcolor:"rgba(0,0,0,1)",
       ticklen:3.6,
       tickwidth:0.66,
       showticklabels:true,
       tickfont:{
         color:"rgba(0,0,0,1)",
         family:"Dorid Sans",
         size:15
       },
       tickangle:0,
       showline:false,
       linecolor:null,
       linewidth:0,
       showgrid:true,
       domain:[0,1],
       gridcolor:null,
       gridwidth:0,
       zeroline:false,
       anchor:"x",
       title:ylab,
       titlefont:{
         color:"rgba(0,0,0,1)",
         family:"Dorid Sans",
         size:18
       }
     },
     shapes:[{
       type:"rect",
       fillcolor:"transparent",
       line:{
         color:"rgba(0,0,0,1)",
         width:0,
         linetype:"solid"
       },
       yref:"paper",
       xref:"paper",
       x0:0,
       x1:1,
       y0:0,
       y1:1
     }],
     showlegend:true,
     legend:{
       bgcolor:null,
       bordercolor:null,
       corderwidth:0,
       font:{
         color:"rgba(0,0,0,1)",
         family:"Dorid Sans",
         size:12
       },
       y:0.959
     },
     hovermode:"closest"
    }
    layout.width = 800
    layout.height = 600

//Plotly.newPlot('score_plot', data, layout);
    return({data:data, layout:layout})
}


// color_by = unpack(ooo.f,"knownORunknown")
// shape_by = unpack(ooo.f,"knownORunknown")
// color_option = ["rgba(0,175,187,1)", "rgba(252,78,7,1)"]
// shape_option = ["circle", "square", "diamond", "cross"]
// loadings = jStat.transpose(oo.loadings)
// x = loadings[0]
// y = loadings[1]
// variance = oo.variance
// color_levels = ["known", "unknown"]
// shape_levels = ["known", "unknown"]
// labels = unpack(ooo.f, 'label')
// scatter_size = 6
// xlab = "PC 1 (1%)"
// ylab = "PC 2 (1%)"
// add_center = true
// ellipse_shape = false
// ellipse_color = false
loading_plot = function(x,y,xlab,ylab,variance, color_by, shape_by, color_option, shape_option, color_levels, shape_levels, labels, scatter_size, add_center, ellipse_color, ellipse_shape){


  var ellipse_color = ellipse_color == true
  var ellipse_shape = ellipse_shape == true
  var add_center = add_center == true


    if(ellipse_color==false){
      var ellipse_color_by = Array(color_by.length).fill("")
      var ellipse_color_levels = [""]
    }else{
      var ellipse_color_by = color_by
      var ellipse_color_levels = color_levels
    }
    if(ellipse_shape==false){
      var ellipse_shape_by = Array(shape_by.length).fill("")
      var ellipse_shape_levels = [""]
    }else{
      var ellipse_shape_by = shape_by
      var ellipse_shape_levels = shape_levels
    }

    if(shape_by.filter(unique)[0]===undefined){
      var shape_by = Array(shape_by.length).fill("")
      var shape_levels = [""]
    }
    if(color_by.filter(unique)[0]===undefined){
      var color_by = Array(color_by.length).fill("")
      var color_levels = [""]
    }



    var color = revalue(color_by, color_levels,color_option)
    var shape = revalue(shape_by, shape_levels,shape_option)

    var ellipse_colors = revalue(ellipse_color_by, ellipse_color_levels,color_option)
    var ellipse_shapes = revalue(ellipse_shape_by, ellipse_shape_levels,shape_option)

    var data = [];
    var group = color.map((x,i)=> x+"+"+shape[i])

    var grouped_loadings_x = groupData(group,x)
    var grouped_loadings_y = groupData(group,y)
    var grouped_labels = groupData(group,labels)
    var grouped_color = groupData(group,color)
    var grouped_shape = groupData(group,shape)


    // deterine if ellipse is needed
    var ellipse_group = ellipse_colors.map((x,i)=> x+"+"+ellipse_shapes[i])
    if(ellipse_color && ellipse_shape){
      ellipse_group_x =  groupData(ellipse_group,x)
      ellipse_group_y =  groupData(ellipse_group,y)
    }else if(ellipse_color){
      //var ellipse_group = color.map((x,i)=> x)
      ellipse_group_x =  groupData(ellipse_group,x)
      ellipse_group_y =  groupData(ellipse_group,y)
    }else if(ellipse_shape){
      //var ellipse_group = shape.map((x,i)=> x)
      ellipse_group_x =  groupData(ellipse_group,x)
      ellipse_group_y =  groupData(ellipse_group,y)
    }else{
      //var ellipse_group = null
      ellipse_group_x = {}
      ellipse_group_y = {}
    }

    // these remeber the minimum and maximum value of each ellipse so that the range of xaxis and y axsis can be moderate.
    min_x_ellipse = 0
    max_x_ellipse = 0
    min_y_ellipse = 0
    max_y_ellipse = 0


    // add ellipse
    for(var ellipse_groupi=0;ellipse_groupi<Object.keys(ellipse_group_x).length;ellipse_groupi++){
       var ellipse_group_type = Object.keys(ellipse_group_x)[ellipse_groupi]
       var ellipse_dta = ellipse(ellipse_group_x[ellipse_group_type],ellipse_group_y[ellipse_group_type]);
       if(ellipse_dta.x !== null){
         min_x_ellipse = jStat.min([min_x_ellipse,jStat.min(ellipse_dta[0])])
         max_x_ellipse = jStat.max([max_x_ellipse,jStat.max(ellipse_dta[0])])
         min_y_ellipse = jStat.min([min_y_ellipse,jStat.min(ellipse_dta[0])])
         max_y_ellipse = jStat.max([min_y_ellipse,jStat.max(ellipse_dta[0])])
       }else{
         ellipse_dta = [null,null]
       }

         data.push({
           x:ellipse_dta[0],
           y:ellipse_dta[1],
           text:null,
           type:"scatter",
           mode:"lines",
           line:{
             width:1.889764,
             color:transparent_rgba(ellipse_group_type.split("+")[0],0.1),
             dash:"solid"
           },
           fill:"toself",
           fillcolor:transparent_rgba(ellipse_group_type.split("+")[0],0.1),
           name:"",
           legendgroup:color_levels[color_option.indexOf(grouped_color[ellipse_group_type][0])] + " " + shape_levels[shape_option.indexOf(grouped_shape[ellipse_group_type][0])],
           showlegend:false,
           xaxis:"x",
           yaxis:"y",
           hoverinfo:"skip"
         })
     }




    // data
    for(var groupi=0;groupi<Object.keys(grouped_loadings_x).length;groupi++){
      var group_type = Object.keys(grouped_loadings_x)[groupi]
      if(shape_levels[shape_option.indexOf(grouped_shape[group_type][0])]==''){
        var name = color_levels[color_option.indexOf(grouped_color[group_type][0])]
      }else if(color_levels[color_option.indexOf(grouped_color[group_type][0])]==''){
        var name = shape_levels[shape_option.indexOf(grouped_shape[group_type][0])]
      }else{
        var name = color_levels[color_option.indexOf(grouped_color[group_type][0])] + "<br />" + shape_levels[shape_option.indexOf(grouped_shape[group_type][0])]
      }

          data.push({
            x:grouped_loadings_x[group_type],
            y:grouped_loadings_y[group_type],
            text:grouped_labels[group_type],
            type:"scatter",
            mode:"markers",
            marker:{
              autocolorscale:false,
              color:group_type.split("+")[0],
              opacity:1,
              size:scatter_size,
              symbol:group_type.split("+")[1],
              line:{
                width:1,
                color:"rgba(0,0,0,1)"
              }
            },
            hoveron:"points",
            name:name,
            legendgroup:color_levels[color_option.indexOf(grouped_color[group_type][0])] + " " + shape_levels[shape_option.indexOf(grouped_shape[group_type][0])],
            showlegend:name!=="",
            xaxis:"x",
            yaxis:"y",
            hoverinfo:"text"
         })
         if(add_center){
          data.push({
            x:[jStat.median(grouped_loadings_x[group_type])],
            y:[jStat.median(grouped_loadings_y[group_type])],
            text:"center of "+color_levels[color_option.indexOf(grouped_color[group_type][0])] + " " + shape_levels[shape_option.indexOf(grouped_shape[group_type][0])],
            type:"scatter",
            mode:"markers",
            marker:{
              autocolorscale:false,
              color:group_type.split("+")[0],
              opacity:1,
              size:scatter_size*2,
              symbol:group_type.split("+")[1],
              line:{
                width:1,
                color:"rgba(0,0,0,1)"
              }
            },
            hoveron:"points",
            name:color_levels[color_option.indexOf(grouped_color[group_type][0])] + "<br />" + shape_levels[shape_option.indexOf(grouped_shape[group_type][0])],
            legendgroup:color_levels[color_option.indexOf(grouped_color[group_type][0])] + " " + shape_levels[shape_option.indexOf(grouped_shape[group_type][0])],
            showlegend:false,
            xaxis:"x",
            yaxis:"y",
            hoverinfo:"text"
         })
         }
    }

    // x axis bound
    var bounds_x = nice_bounds(jStat.min([min_x_ellipse,jStat.min(x)]), jStat.max([max_x_ellipse,jStat.max(x)]))
    var bounds_y = nice_bounds(jStat.min([min_y_ellipse,jStat.min(y)]), jStat.max([max_y_ellipse,jStat.max(y)]))

    // x/y zero line
    data.push({
      x:[bounds_x.min,bounds_x.max],
      y:[0,0],
      type:"scatter",
      line:{
        width:1,
        color:"rgba(0,0,0,1)",
        dash:"dash"
      },
      hoveron:"points",
      showlegend:false,
      xaxis:"x",
      yaxis:"y",
      hoverinfo:"skip",
      name:""
    })
    data.push({
      x:[0,0],
      y:[bounds_y.min,bounds_y.max],
      type:"scatter",
      line:{
        width:1,
        color:"rgba(0,0,0,1)",
        dash:"dash"
      },
      hoveron:"points",
      showlegend:false,
      xaxis:"x",
      yaxis:"y",
      hoverinfo:"skip",
      name:""
    })

    // layout
    layout = {
      /*margin:{
       t:40.57658,
       r:18.99543,
       b:42.84142,
       l:48.94977
      },*/
      font:{
        color:"rgba(0,0,0,1)",
        family:"Dorid Sans",
        size:15
      },
      title:"<b>Loading Plot</b>",
      titlefont:{
        color:"rgba(0,0,0,1)",
        family:"Dorid Sans",
        size:20
      },
      plot_bgcolor:"rgba(255,255,255,1)",
      paper_bgcolor:"rgba(255,255,255,1)",
      xaxis:{
       type:"linear",
       autorange:true,
       //range:[bounds_x.min,bounds_x.max],
       tickmode:"array",
       //ticktext:_.range(bounds_x.min, bounds_x.max, bounds_x.steps).map(x => x<0? Number(x.toFixed(2)).toExponential():x.toFixed(2)),
       //tickvals:_.range(bounds_x.min, bounds_x.max, bounds_x.steps),
       //categoryorder:"array",
       //categoryarray:_.range(bounds_x.min, bounds_x.max, bounds_x.steps),
       //nticks:null,
       ticks:"outside",
       tickcolor:"rgba(0,0,0,1)",
       ticklen:3.6,
       tickwidth:0.66,
       showticklabels:true,
       tickfont:{
         color:"rgba(0,0,0,1)",
         family:"Dorid Sans",
         size:15
       },
       tickangle:0,
       showline:false,
       linecolor:null,
       linewidth:0,
       showgrid:true,
       domain:[0,1],
       gridcolor:null,
       gridwidth:0,
       zeroline:false,
       anchor:"y",
       title:xlab,
       titlefont:{
         color:"rgba(0,0,0,1)",
         family:"Dorid Sans",
         size:18
       }
     },
     yaxis:{
       type:"linear",
       autorange:true,
       //range:[bounds_y.min,bounds_y.max],
       tickmode:"array",
       //ticktext:_.range(bounds_y.min, bounds_y.max,bounds_y.steps).map(x => x<0? Number(x.toFixed(2)).toExponential():x.toFixed(2)),
       //tickvals:_.range(bounds_y.min, bounds_y.max,bounds_y.steps),
       //categoryorder:"array",
       //categoryarray:_.range(bounds_y.min, bounds_y.max,bounds_y.steps),
       //nticks:null,
       ticks:"outside",
       tickcolor:"rgba(0,0,0,1)",
       ticklen:3.6,
       tickwidth:0.66,
       showticklabels:true,
       tickfont:{
         color:"rgba(0,0,0,1)",
         family:"Dorid Sans",
         size:15
       },
       tickangle:0,
       showline:false,
       linecolor:null,
       linewidth:0,
       showgrid:true,
       domain:[0,1],
       gridcolor:null,
       gridwidth:0,
       zeroline:false,
       anchor:"x",
       title:ylab,
       titlefont:{
         color:"rgba(0,0,0,1)",
         family:"Dorid Sans",
         size:18
       }
     },
     shapes:[{
       type:"rect",
       fillcolor:"transparent",
       line:{
         color:"rgba(0,0,0,1)",
         width:0,
         linetype:"solid"
       },
       yref:"paper",
       xref:"paper",
       x0:0,
       x1:1,
       y0:0,
       y1:1
     }],
     showlegend:true,
     legend:{
       bgcolor:null,
       bordercolor:null,
       corderwidth:0,
       font:{
         color:"rgba(0,0,0,1)",
         family:"Dorid Sans",
         size:12
       },
       y:0.959
     },
     hovermode:"closest"
    }
    layout.width = 800
    layout.height = 600

//Plotly.newPlot('loading_plot', data, layout);
    return({data:data, layout:layout})
}


function nice_number(value, round_){
    //default value for round_ is false
    round_ = round_ || false;
    // :latex: \log_y z = \frac{\log_x z}{\log_x y}
    var exponent = Math.floor(Math.log(value) / Math.log(10));
    var fraction = value / Math.pow(10, exponent);

    if (round_)
        if (fraction < 1.5)
            nice_fraction = 1.
        else if (fraction < 3.)
            nice_fraction = 2.
        else if (fraction < 7.)
            nice_fraction = 5.
        else
            nice_fraction = 10.
    else
        if (fraction <= 1)
            nice_fraction = 1.
        else if (fraction <= 2)
            nice_fraction = 2.
        else if (fraction <= 5)
            nice_fraction = 5.
        else
            nice_fraction = 10.

    return nice_fraction * Math.pow(10, exponent)
}

function nice_bounds(axis_start, axis_end, num_ticks){
    //default value is 10
    num_ticks = num_ticks || 10;
    var axis_width = axis_end - axis_start;

    if (axis_width == 0){
        axis_start -= .5
        axis_end += .5
        axis_width = axis_end - axis_start
    }

    var nice_range = nice_number(axis_width);
    var nice_tick = nice_number(nice_range / (num_ticks -1), true);
    var axis_start = Math.floor(axis_start / nice_tick) * nice_tick;
    var axis_end = Math.ceil(axis_end / nice_tick) * nice_tick;
    return {
        "min": axis_start,
        "max": axis_end,
        "steps": nice_tick
    }
}



FDR_options =  [
     {
         "id": "holm",
         "text": "Holm (1979)"
     },
     {
         "id": "hochberg",
         "text": "Hochberg (1988)"
     },
     {
         "id": "hommel",
         "text": "Hommel (1988)"
     },
     {
         "id": "bonferroni",
         "text": "Bonferroni correction"
     },
     {
         "id": "BH",
         "text": "Benjamini & Hochberg (1995)"
     },
     {
         "id": "BY",
         "text": "Benjamini & Yekutieli (2001)"
     },
     {
         "id": "fdr",
         "text": "FDR correction (aka Benjamini & Hochberg)"
     },
     {
         "id": "none",
         "text": "No correction"
     }
 ]
scale_options =  [
               {
                   "id": "standard",
                   "text": "Auto Scaling"
               },
               {
                   "id": "pareto",
                   "text": "Pareto Scaling"
               },
               {
                   "id": "center",
                   "text": "Mean Centering"
               },
               {
                   "id": "none",
                   "text": "No Scaling"
               }
           ]
scree_plot = function(y){
  var x = Array.from({length: y.length}, (v, k) => k+1);
  var text = y.map(x => (x*100).toFixed(2)+"%" )
  var hovertext = text.map((x,i) => "PC"+ (i+1)+": "+x)

var bar_trace = {
    orientation: "v",
    width: 0.9,
    base:0,
    x: x,
    y: y,
    text:text,
    type:'bar',
    marker:{
      autocolorscale:false,
      color:"rgba(70,130,180,1)",
      line:{
        width:2,
        color:"rgba(70,130,180,1)"
      }
    },
    showlegend:false,
    xaxis:"x",
    yaxis:"y",
    hoverinfo:"text",
    mode:"",
    name:""
  };
var line_trace = {
    x: x,
    y: y,
    text:text,
    mode:"lines+markers",
    line:{
      width:2,
      color:"rgba(0,0,0,1)",
      dash:"solid"
    },
    hoveron:"points",
    showlegend:false,
    xaxis:"x",
    yaxis:"y",
    hoverinfo:"text",
    marker:{
      autocolorscale:false,
      color:"rgba(0,0,0,1)",
      opacity:1,
      size:6,
      symbol:"circle",
      line:{
        width:2,
        color:"rgba(0,0,0,1)"
      }
    },
    name:""
  };
var scatter_trace = {
    x: x.map(x=>x+0.3),
    y: y.map(x=>x+0.01),
    text:text,
    hovertext:hovertext,
    textfont:{
      size:15,
      color:"rgba(0,0,0,1)",
    },
    type: 'scatter',
    mode:"text",
    hoveron:"points",
    showlegend:false,
    xaxis:"x",
    yaxis:"y",
    hoverinfo:"text",
    name:""
  };
var layout = {
  /*margin:{
    t:42,
    r:7.3,
    b:42,
    l:60
  },*/
  font:{
    color:"rgba(0,0,0,1)",
    family:"Dorid Sans",
    size:18
  },
  title:"Scree Plot",
  titlefont:{
    color:"rgba(0,0,0,1)",
    family:"Dorid Sans",
    size:25
  },
  xaxis:{
    type:"linear",
    autorange:false,
    range:[0.4,10.6],
    tickmode:"array",
    ticktext:x,
    tickvals:x,
    categoryorder:"array",
    categoryarray:x,
    nticks:null,
    ticks:"outside",
    tickcolor:"rbga(0,0,0,1)",
    ticklen:3.65,
    tickwidth:0.66,
    showticklabels:true,
    tickfont:{
      color:"rgba(0,0,0,1)",
      family:"Dorid Sans",
      size:15,
      tickangle:0
    },
    showline:false,
    linecolor:null,
    linewidth:0,
    showgrid:true,
    domain:[0,1],
    gridcolor:"rgba(0,0,0,0)",
    gridwidth:0,
    zeroline:false,
    anchor:"y",
    title:"Number of Principal Components",
    titlefont:{
      color:"rgba(0,0,0,1)",
      family:"Dorid Sans",
      size:18
    },
    hoverformat:".2f"
  },
  yaxis:{
    type:"linear",
    autorange:false,
    range:[-0.01,
    y.reduce(function(a, b) {
                return Math.max(a, b);
            })*1.1],
    tickmode:"array",
    categoryorder:"array",
    nticks:null,
    ticks:"outside",
    tickcolor:"rgba(0,0,0,1)",
    ticklen:3.65,
    tickwidth:0.66,
    showticklabels:true,
    tickfont:{
      color:"rgba(0,0,0,1)",
      family:"Dorid Sans",
      size:15
    },
    tickangle:0,
    showline:false,
    linecolor:"rgba(0,0,0,1)",
    showgrid:true,
    domain:[0,1],
    gridcolor:"rgba(0,0,0,0.2)",
    gridwidth:0,
    zeroline:false,
    anchor:"x",
    title:"Percentage of Explained Variances",
    titlefont:{
      color:"rgba(0,0,0,1)",
      family:"Dorid Sans",
      size:18
    },
    hoverformat:".2f"
  },
  shapes:[
    {
      type:"rect",
      fillcolor:null,
      line:{
        color:'rgba(0,0,0,1)',
        width:1,
        linetype:"solid"
      },
      yref:"paper",
      xref:"paper",
      x0:0,
      x1:1,
      y0:0,
      y1:1
    },
  ],
  showlegend:false,
  hovermode:"closest",
  barmode:"relative",
  height:600,
  width:800
};
  return({data:[bar_trace, line_trace, scatter_trace], layout:layout})
}



scree_plot_plsda = function(y1,y2,y3){

  var x = Array.from({length: y1.length}, (v, k) => k+1);
  var text1 = y1.map(x => (x*100).toFixed(2)+"%" )
  var text2 = y2.map(x => (x*100).toFixed(2)+"%" )
  var text3 = y3.map(x => (x*100).toFixed(2)+"%" )
  var hovertext1 = text1.map((x,i) => "PC"+ (i+1)+": "+x)
  var hovertext2 = text2.map((x,i) => "PC"+ (i+1)+": "+x)
  var hovertext3 = text3.map((x,i) => "PC"+ (i+1)+": "+x)

var bar_trace1 = {
    orientation: "v",
    //width: 0.9,
    base:0,
    x: x,
    y: y1,
    text:text1,
    type:'bar',
    marker:{
      autocolorscale:false,
      color:"rgba(89,181,199,1)",
      line:{
        width:2,
        color:"rgba(89,181,199,1)"
      }
    },
    showlegend:false,
    xaxis:"x",
    yaxis:"y",
    hoverinfo:"text",
    mode:"",
    name:""
  };
var bar_trace2 = {
    orientation: "v",
    //width: 0.9,
    base:0,
    x: x,
    y: y2,
    text:text2,
    type:'bar',
    marker:{
      autocolorscale:false,
      color:"rgba(235,88,81,1)",
      line:{
        width:2,
        color:"rgba(235,88,81,1)"
      }
    },
    showlegend:false,
    xaxis:"x",
    yaxis:"y",
    hoverinfo:"text",
    mode:"",
    name:""
  };
var bar_trace3 = {
    orientation: "v",
    //width: 0.9,
    base:0,
    x: x,
    y: y3,
    text:text3,
    type:'bar',
    marker:{
      autocolorscale:false,
      color:"rgba(248,215,120,1)",
      line:{
        width:2,
        color:"rgba(248,215,120,1)"
      }
    },
    showlegend:false,
    xaxis:"x",
    yaxis:"y",
    hoverinfo:"text",
    mode:"",
    name:""
  };
var line_trace = {
    x: x,
    y: y2,
    text:text2,
    mode:"lines+markers",
    line:{
      width:2,
      color:"rgba(0,0,0,1)",
      dash:"solid"
    },
    hoveron:"points",
    showlegend:false,
    xaxis:"x",
    yaxis:"y",
    hoverinfo:"text",
    marker:{
      autocolorscale:false,
      color:"rgba(0,0,0,1)",
      opacity:1,
      size:6,
      symbol:"circle",
      line:{
        width:2,
        color:"rgba(0,0,0,1)"
      }
    },
    name:""
  };
var scatter_trace = {
    x: x.map(x=>x+0.3),
    y: y2.map(x=>x+0.02),
    text:text2,
    hovertext:hovertext2,
    textfont:{
      size:15,
      color:"rgba(0,0,0,1)",
    },
    type: 'scatter',
    mode:"text",
    hoveron:"points",
    showlegend:false,
    xaxis:"x",
    yaxis:"y",
    hoverinfo:"text",
    name:""
  };
var layout = {
  /*margin:{
    t:42,
    r:7.3,
    b:42,
    l:60
  },*/
  font:{
    color:"rgba(0,0,0,1)",
    family:"Dorid Sans",
    size:18
  },
  title:"Scree Plot",
  titlefont:{
    color:"rgba(0,0,0,1)",
    family:"Dorid Sans",
    size:25
  },
  xaxis:{
    type:"linear",
    autorange:false,
    range:[0.4,10.6],
    tickmode:"array",
    ticktext:x,
    tickvals:x,
    categoryorder:"array",
    categoryarray:x,
    nticks:null,
    ticks:"outside",
    tickcolor:"rbga(0,0,0,1)",
    ticklen:3.65,
    tickwidth:0.66,
    showticklabels:true,
    tickfont:{
      color:"rgba(0,0,0,1)",
      family:"Dorid Sans",
      size:15,
      tickangle:0
    },
    showline:false,
    linecolor:null,
    linewidth:0,
    showgrid:true,
    domain:[0,1],
    gridcolor:"rgba(0,0,0,0)",
    gridwidth:0,
    zeroline:false,
    anchor:"y",
    title:"Number of Pridictive Components",
    titlefont:{
      color:"rgba(0,0,0,1)",
      family:"Dorid Sans",
      size:18
    },
    hoverformat:".2f"
  },
  yaxis:{
    type:"linear",
    autorange:true,
    /*range:[-0.01,
    y.reduce(function(a, b) {
                return Math.max(a, b);
            })*1.1],*/
    tickmode:"array",
    categoryorder:"array",
    nticks:null,
    ticks:"outside",
    tickcolor:"rgba(0,0,0,1)",
    ticklen:3.65,
    tickwidth:0.66,
    showticklabels:true,
    tickfont:{
      color:"rgba(0,0,0,1)",
      family:"Dorid Sans",
      size:15
    },
    tickangle:0,
    showline:false,
    linecolor:"rgba(0,0,0,1)",
    showgrid:true,
    domain:[0,1],
    gridcolor:"rgba(0,0,0,0.2)",
    gridwidth:0,
    zeroline:false,
    anchor:"x",
    title:"Percentage of X2, R2 and Q2",
    titlefont:{
      color:"rgba(0,0,0,1)",
      family:"Dorid Sans",
      size:18
    },
    hoverformat:".2f"
  },
  shapes:[
    {
      type:"rect",
      fillcolor:null,
      line:{
        color:'rgba(0,0,0,1)',
        width:1,
        linetype:"solid"
      },
      yref:"paper",
      xref:"paper",
      x0:0,
      x1:1,
      y0:0,
      y1:1
    },
  ],
  showlegend:false,
  hovermode:"closest",
  barmode:"group"
};
  return({data:[bar_trace1, bar_trace2, bar_trace3, line_trace, scatter_trace], layout:layout})
}

download_csv = function(string, filename = 'result.csv'){
  var blob = new Blob([string]);
  var a = window.document.createElement("a");
    a.href = window.URL.createObjectURL(blob, {type: "text/plain"});
    a.download = filename;
    document.body.appendChild(a);
    a.click();  // IE: "Access is denied"; see:
}
function sort(arr,desending=false) {
  if(desending){
    return arr.concat().sort(function(a, b){return b-a});
  }else{
    return arr.concat().sort();
  }

}
delete_element_from_array = function(array,element){
  var result = []
  for(var i=0;i<array.length;i++){
    if(array[i]!==element){
      result.push(array[i])
    }
  }
  return(result)
}
get_time_string = function(){
	var d = new Date();var time_string = d.getTime().toString()
	return(time_string)
}

function getAllIndexes(arr, val) {
    var indexes = [], i;
    for(i = 0; i < arr.length; i++)
        if (arr[i] === val)
            indexes.push(i);
    return indexes;
} // get all the indexes of value in an array https://stackoverflow.com/questions/20798477/how-to-find-index-of-all-occurrences-of-element-in-array

function collapseSection(element) {
  // get the height of the element's inner content, regardless of its actual size
  var sectionHeight = element.scrollHeight;

  // temporarily disable all css transitions
  var elementTransition = element.style.transition;
  element.style.transition = '';

  // on the next frame (as soon as the previous style change has taken effect),
  // explicitly set the element's height to its current pixel height, so we
  // aren't transitioning out of 'auto'
  requestAnimationFrame(function() {
    element.style.height = sectionHeight + 'px';
    element.style.transition = elementTransition;

    // on the next frame (as soon as the previous style change has taken effect),
    // have the element transition to height: 0
    requestAnimationFrame(function() {
      element.style.height = 0 + 'px';
    });
  });

  // mark the section as "currently collapsed"
  element.setAttribute('data-collapsed', 'true');
}

function expandSection(element) {

    setTimeout(function(){
        // get the height of the element's inner content, regardless of its actual size
        var sectionHeight = element.scrollHeight;

        // have the element transition to the height of its inner content
        element.style.height = sectionHeight + 'px';

        // when the next css transition finishes (which should be the one we just triggered)
        element.addEventListener('transitionend', function(e) {
        // remove this event listener so it only gets triggered once
        element.removeEventListener('transitionend', arguments.callee);

        // remove "height" from the element's inline styles, so it can return to its initial value
        element.style.height = null;
        });

        // mark the section as "currently not collapsed"
        element.setAttribute('data-collapsed', 'false');
    }, 3);


}

function unpack(rows, key) {

  for(var i=0; i<rows.length;i++){
    if(rows[i] === null){
      rows[i] = ""
    }
  }

    return rows.map(function(row)
    { return row[key]; });
}// unpack(data,key)



subset_array_by_a_in_b = function(array,a,b){
  rrr = array
  aaa = a
  bbb = b

  if(typeof(a) == 'string'){
    a = [a]
  }
  if(typeof(array)=='string'){
    array= [array]
  }


  if(array !== undefined){
    var index = a.map(x => b.includes(x) || b.includes(Number(x)))
    var result = []


    for(var i=0;i<array.length;i++){
      if(index[i]){
        result.push(array[i])
      }
    }


  }else{
    result = Array(b.length).fill("sudo")
  }


  return(result)
} // subset the array by checking which element in a is in b. subset_array_by_a_in_b(factor1,a,b)

function unique(value, index, self) {
    return self.indexOf(value) === index;
} // [].filter(unique)


array_split_by_one_factor = function(array,fac,lev){
  //fac = unpack(p.data,"species")
  var result = [];
  for(var l1=0; l1<lev.length;l1++){
      var target_level = lev[l1]
      result.push(getAllIndexes(fac,target_level).map(ind => array[ind]))
  }
  return(result)
}

get_one_way_boxplot_data = function(array,factor,level, shown_level,regexp){
                        dataset = array_split_by_one_factor(array,factor,level)
                        var data =[]
                        for(var i =0;i<dataset.length;i++){
                          data.push({
                            y:dataset[i],
                            type:'box',
                            boxpoints: 'all',
                            jitter: 0.3,
                            pointpos: -1.8,
                            name:level[i],
                            visible:shown_level.indexOf(regexp ? level[i].replace(/[^a-zA-Z0-9]/g,'_') : level[i]) !== -1 ? true:'legendonly'
                          })
                        }
                        return(data)
                      }



      load_tree = function(tree_structure){
        $('#jstree').jstree('destroy');
          $("#jstree").jstree({
            // load the tree from database
            'core':{
              'data':tree_structure,
              'multiple':false, // cannot select multiple nodes.
              'expand_selected_onload':true,
              'check_callback' : true
            },
            'contextmenu':{ // content when user right click a node.
              "show_at_node":true, // the menu follows the mouse.
              'items':function($node){
                clicked_node = $node
                // define what can be done on a node.
                var gotoable = false;

                if($node.icon == 'fa fa-folder'){
                  gotoable = true
                }
                /*var createable = false;// true when folder.
                var renameable = true;// cannot remove or rename the sample info compound info and expresstion data
                var removeable = true;// cannot remove or rename the sample info compound info and expresstion data
                var uploadable = false;// true when folder.
                var downloadable = true;
                var editable = false;
                if($node.parent == '#'){// if node is the toppest parent node, cannot be removed and cannot be renamed.
                    renameable = false;
                    removeable = false;
                  }
                if($node.icon == "fa fa-folder"){
                    uploadable = true;
                    createable = true;
                }
                if(($node.text === "sample_info.csv" && $node.parent === "root") || ($node.text === "compound_data.csv" && $node.parent === "root") || ($node.text === "expression_data.csv"&& $node.parent === "root")){
                  renameable = false;
                  removeable = false;
                }
                if(($node.text === "sample_info.csv" && $node.parent === "root") || ($node.text === "compound_data.csv" && $node.parent === "root")){
                  var editable = true;
                }
                var sourceable = true;// able to see the source of a file (to see which calculation ends with this file.)*/
                var tree = $("#jstree").jstree(true);
                var items = {
                  "goto":{
                    "label":"Go To",
                    'icon':"fa fa-paper-plane-o",
                    '_disabled':!gotoable,
                    'action':function(obj){
                      ooo = obj
                      nnn = $node
                      $('#one').animate({
                    		scrollTop: $("#"+$node.id).offset().top
                    	},500, "swing");
                    }
                  }
                  /*'Create':{
                    'label':"Create Folder",
                    'icon':"fa fa-plus-square-o",
                    '_disabled':!createable,
                    'action':function(obj){
                      $node = tree.create_node($node);
                      tree.edit($node, null, function(node){// check if the node's new name has been taken. If so, delete this node. Otherwise, create a new node, with id 'newname'+Data().
                        $('#project_structure_ibox').children('.ibox-content').addClass('sk-loading');
                        db = new PouchDB('https://tempusername:temppassword@metda.fiehnlab.ucdavis.edu/db/abib');
                        db.get(project_id).then(function(doc){
                          var nd = node;
                          var sibling_id = tree.get_node(nd.parent).children
                          var sibling_name = [];
                          for(var i=0;i<sibling_id.length;i++){
                            sibling_name.push(sibling_id[i].split("_68410298_")[0])
                          }
                          if(sibling_name.indexOf(nd.text) > -1){
                            tree.delete_node(node);
                            alert("The name, '"+nd.text+"' has been taken.")
                            $('#project_structure_ibox').children('.ibox-content').removeClass('sk-loading');
                          }else{
                            var d = new Date();
                            var num_date = d.getTime();
                            doc.tree_structure.push({
                              "id":nd.text+"_68410298_"+num_date,
                              "parent":nd.parent,"text":nd.text,
                              "icon":"fa fa-folder"
                            })
                            // reload the tree.
                            var db = new PouchDB('https://tempusername:temppassword@metda.fiehnlab.ucdavis.edu/db/abib');db.put(doc).then(function(){$scope.load_tree(project_id);$('#project_structure_ibox').children('.ibox-content').removeClass('sk-loading');});
                          }
                        }).catch(function (error) {
                              alert(error)
                              $('#project_structure_ibox').children('.ibox-content').removeClass('sk-loading');
                          });
                      });
                    }
                  },
                  "Rename": {
                      "label": "Rename",
                      "icon":"fa fa-edit",
                      "_disabled": !renameable,
                      "action": function (obj) {
                        tree.edit($node, null, function(node){
                          $('#project_structure_ibox').children('.ibox-content').addClass('sk-loading');
                          var old_node = $node
                          db = new PouchDB('https://tempusername:temppassword@metda.fiehnlab.ucdavis.edu/db/abib');
                          db.get(project_id).then(function(doc){
                            var nd = node;
                            var sibling_id = tree.get_node(nd.parent).children
                            var sibling_name = [];
                            for(var i=0;i<sibling_id.length;i++){
                              sibling_name.push(sibling_id[i].split("_68410298_")[0])
                            }
                            if(sibling_name.indexOf(nd.text) > -1){
                              alert(nd.text + " is taken!")
                              $('#jstree').jstree(true).rename_node($('#jstree').jstree('get_selected'), old_node.id.split("_68410298_")[0]);
                              $('#project_structure_ibox').children('.ibox-content').removeClass('sk-loading');
                            }else{
                              // change ID of old node.
                              // change parent of old node children.
                              var d = new Date();
                              var num_date = d.getTime();
                              var new_id = nd.text+"_68410298_"+num_date
                              // delett the old node.
                              for(var i=0;i<doc.tree_structure.length;i++){
                                if(doc.tree_structure[i].id === old_node.id && doc.tree_structure[i].parent === old_node.parent){
                                  var old_tree_info = doc.tree_structure[i]
                                  doc.tree_structure.splice(i,1); break
                                }
                              }
                              // add siblings with new id.
                              for(var i=0;i<doc.tree_structure.length;i++){
                                if(doc.tree_structure[i].parent === old_node.id){
                                  doc.tree_structure[i].parent = new_id
                                }
                              }
                              // new id as parent.
                              doc.tree_structure.push({
                                "id":new_id,
                                "parent":nd.parent,
                                "text":nd.text,
                                "icon":old_tree_info.icon,
                                "source":old_tree_info.source,
                                "attname":old_tree_info.attname,
                                "column_name":old_tree_info.column_name,
                                "column_length":old_tree_info.column_length,
                                "column_class":old_tree_info.column_class,
                                "column_value":old_tree_info.column_value,
                              })
                              var db = new PouchDB('https://tempusername:temppassword@metda.fiehnlab.ucdavis.edu/db/abib');db.put(doc).then(function(){$scope.load_tree(project_id);$('#project_structure_ibox').children('.ibox-content').removeClass('sk-loading');});
                            }
                          }).catch(function (error) {
                                alert(error)
                                $('#project_structure_ibox').children('.ibox-content').removeClass('sk-loading');
                            });
                        });
                      }
                  },
                  "Remove": {
                    "label": "Remove",
                    "icon":"fa fa-trash-o",
                    "_disabled":!removeable,
                    "action": function (obj) {
                        $scope.delete_node_confirm(project_id)
                        // confirm_delete_node
                    }
                  },
                  "Edit":{
                    "label": "Edit",
                    "icon":"fa fa-edit",
                    "_disabled":!editable,
                    "action":function(obj){
                      jjj=obj
                      console.log(obj)

                      jjj.reference[0].id
                      if(obj.reference[0].id.indexOf("sample_info")===-1){
                        $scope.edit_node_confirm(project_id,'compound')
                      }else{
                        $scope.edit_node_confirm(project_id,'sample')
                      }

                      // confirm_edit_node
                    }
                  },
                  "Download":{
                     "label": "Download",
                     "icon":"fa fa-download",
                    "_disabled":!downloadable,
                    "action":function(obj){
                      // download the file.
                      console.log($node.id)
                      if($node.id.indexOf("_csv") !== -1){
                        window.open("http://tempusername:temppassword@metda.fiehnlab.ucdavis.edu/db/abib/"+project_id+"/"+$node.id.replace('+',"%2B").replace("_csv",".csv"))
                      }else if($node.id.indexOf("_xlsx") !== -1){
                        window.open("http://tempusername:temppassword@metda.fiehnlab.ucdavis.edu/db/abib/"+project_id+"/"+$node.id.replace('+',"%2B").replace("_xlsx",".xlsx"))
                      }else if($node.id.indexOf("_pptx") !== -1){
                        window.open("http://tempusername:temppassword@metda.fiehnlab.ucdavis.edu/db/abib/"+project_id+"/"+$node.id.replace('+',"%2B").replace("_pptx",".pptx"))
                      }else if($node.id.indexOf("_png") !== -1){
                        window.open("http://tempusername:temppassword@metda.fiehnlab.ucdavis.edu/db/abib/"+project_id+"/"+$node.id.replace('+',"%2B").replace("_png",".png"))
                      }else if($node.id.indexOf("_svg") !== -1){
                        window.open("http://tempusername:temppassword@metda.fiehnlab.ucdavis.edu/db/abib/"+project_id+"/"+$node.id.replace('+',"%2B").replace("_svg",".svg"))
                      }else if($node.id.indexOf("_html") !== -1){
                        window.open("http://tempusername:temppassword@metda.fiehnlab.ucdavis.edu/db/abib/"+project_id+"/"+$node.id.replace('+',"%2B").replace("_html",".html"))
                      }else if($node.id.indexOf("_pdf") !== -1){
                        window.open("http://tempusername:temppassword@metda.fiehnlab.ucdavis.edu/db/abib/"+project_id+"/"+$node.id.replace('+',"%2B").replace("_pdf",".pdf"))
                      }else{ // this means that the node is a folder.
                        $('#project_structure_ibox').children('.ibox-content').addClass('sk-loading');
                        var tree = $('#jstree').jstree(true);
                        var selected_node_id = clicked_node.id;
                        var unincluded_folder = [];
                        included_path = [];
                        included_id = [];
                        included_path[0] = tree.get_path(tree.get_node(selected_node_id).id,"/")
                        included_id[0] = selected_node_id
                        unincluded_folder[0] = tree.get_node(selected_node_id).id;
                        while(unincluded_folder.length > 0){
                          var update_unincluded_folder = [];
                          for(var i=0;i<unincluded_folder.length;i++){
                            //var children = tree.get_children_dom(tree.get_node(unincluded_folder[i]))
                            var children = tree.get_node(unincluded_folder[i]).children
                            for(var j=0;j<children.length;j++){
                              var child_node = tree.get_node(children[j])
                              if(tree.is_leaf(child_node,"/")){
                                included_id.push(child_node.id)
                                included_path.push(tree.get_path(child_node,"/"))
                              }else{
                                included_id.push(child_node.id)
                                included_path.push(tree.get_path(child_node,"/"))
                                update_unincluded_folder.push(child_node.id)
                              }
                            }
                          }
                          unincluded_folder = update_unincluded_folder
                        }
                        var req = ocpu.call("download_folder_as_zip",{
                          project_id:localStorage.getItem("project_id"),
                          id:included_id,
                          path:included_path
                        }, function(session){
                          session.getObject(function(objj){
                            $('#project_structure_ibox').children('.ibox-content').removeClass('sk-loading');
                            window.open(session.loc + "files/" + objj[0])
                          })
                        }).fail(function(){
                            $('#project_structure_ibox').children('.ibox-content').removeClass('sk-loading');
                            $scope.$apply(function(){$scope.calculating = false})
                            alert("Error: " + req.responseText)
                          })
                      }

                    }
                  },*/
                }
                return items;
              }
            },

            "plugins" : [  "contextmenu",  "state"]
          }).on('ready.jstree', function() {
    $("#jstree").jstree("open_all");
})
      }

function trim (str) {
  return str.replace(/^\s+|\s+$/gm,'');
}

function hexToRgb(hex,alpha) {
    var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
    return result ? 'rgba('+parseInt(result[1], 16)+","+parseInt(result[2], 16)+","+parseInt(result[3], 16)+','+alpha+')' : null;
}// from RGB color to hex code
function rgbaToHex (rgba) {
  if(rgba === null){
    return(null)
  }
    var parts = rgba.substring(rgba.indexOf("(")).split(","),
        r = parseInt(trim(parts[0].substring(1)), 10),
        g = parseInt(trim(parts[1]), 10),
        b = parseInt(trim(parts[2]), 10),
        a = parseFloat(trim(parts[3].substring(0, parts[3].length - 1))).toFixed(2);

        r = r.toString(16).length==1? "0"+r.toString(16):r.toString(16)
        g = g.toString(16).length==1? "0"+g.toString(16):g.toString(16)
        b = b.toString(16).length==1? "0"+b.toString(16):b.toString(16)

        a = (a * 255).toString(16).substring(0,2).length ==1? "0"+(a * 255).toString(16).substring(0,2):(a * 255).toString(16).substring(0,2)


    return ('#' + r + g + b +  a);
}

function UrltoBase64(url, callback) {
    var xhr = new XMLHttpRequest();
    xhr.onload = function() {
        var reader = new FileReader();
        reader.onloadend = function() {
            callback(reader.result);
        }
        reader.readAsDataURL(xhr.response);
    };
    xhr.open('GET', url);
    xhr.responseType = 'blob';
    xhr.send();
}

// t-test click table function
/*ctrl.project_structure.cards[id].result.table.table_click = function(e){
                eee = e
                compound_sudo_label = e.compound_sudo_label
                activated_data = ctrl.project_structure.cards[id].r_parameters.dataset_input
                selected_values = Object.values(activated_data[unpack(activated_data,"compound_sudo_label").indexOf(compound_sudo_label)])
                selected_values.shift();selected_values.pop();// get rid of label and sudo_label
                factor = ctrl.project_structure.cards[id].r_parameters.treatment_column1
                target_id = "content"+ctrl.project_structure.cards[id]._id+"_boxplot"

                plot_data = get_one_way_boxplot_data(selected_values, factor, factor.filter(unique),factor.filter(unique),false)

                setTimeout(function() {
                  var d3 = Plotly.d3;var boxplot = d3.select("div[id='"+target_id+"']");var boxplot_div = boxplot.node();
                  Plotly.newPlot(target_id, plot_data,{title:"test",legend: {"orientation": "h"},showlegend: true,hovermode:"closest"}).then(function(plot){window.onresize = function() {Plotly.Plots.resize(boxplot_div);Plotly.Plots.resize(boxplot_div);}});
                },10)
              }*/























/*for(var i=0;i<3;i++){modelRe = new lll.default({ y: [i,1.05791738,-1.88520976,0.08097500,-0.34754943,-1.39860547,0.02737364,1.58462161,-0.91076627,0.81400619], x:  [1.67507935,0.02362506,0.83475130,0.18857902,0.66884508,-0.28572887,0.34971617,0.47584602,-0.42832105,-0.67095903
]}, ooo);var fitRe = modelRe.predict({x:[1.67507935,0.02362506,0.83475130,0.18857902,0.66884508,-0.28572887,0.34971617,0.47584602,-0.42832105,-0.67095903
], x_cut:[1.67507935,0.02362506,0.83475130,0.18857902,0.66884508,-0.28572887,0.34971617,0.47584602,-0.42832105,-0.67095903
]});console.log(fitRe.fitted)}
*/












//adapted from the LoessInterpolator in org.apache.commons.math
function loess_pairs(pairs, bandwidth)
{
  var xval = pairs.map(function(pair){return pair[0]});
  var yval = pairs.map(function(pair){return pair[1]});
  var res = loess(xval, yval, bandwidth);
  return xval.map(function(x,i){return [x, res[i]]});
}

/*function loess(xval, yval, bandwidth)
{
    function tricube(x) {
        var tmp = 1 - x * x * x;
        return tmp * tmp * tmp;
    }

	var res = [];

	var left = 0;
	var right = Math.floor(bandwidth * xval.length) - 1;

	for(var i in xval)
	{
		var x = xval[i];

		if (i > 0) {
			if (right < xval.length - 1 &&
				xval[right+1] - xval[i] < xval[i] - xval[left]) {
					left++;
					right++;
			}
		}

		var edge;
		if (xval[i] - xval[left] > xval[right] - xval[i])
			edge = left;
		else
			edge = right;

		var denom = Math.abs(1.0 / (xval[edge] - x));

		var sumWeights = 0;
		var sumX = 0, sumXSquared = 0, sumY = 0, sumXY = 0;

		var k = left;
		while(k <= right)
		{
			var xk = xval[k];
			var yk = yval[k];
			var dist;
			if (k < i) {
				dist = (x - xk);
			} else {
				dist = (xk - x);
			}
			var w = tricube(dist * denom);
			var xkw = xk * w;
			sumWeights += w;
			sumX += xkw;
			sumXSquared += xk * xkw;
			sumY += yk * w;
			sumXY += yk * xkw;
			k++;
		}

		var meanX = sumX / sumWeights;
		var meanY = sumY / sumWeights;
		var meanXY = sumXY / sumWeights;
		var meanXSquared = sumXSquared / sumWeights;

		var beta;
		if (meanXSquared == meanX * meanX)
			beta = 0;
		else
			beta = (meanXY - meanX * meanY) / (meanXSquared - meanX * meanX);

		var alpha = meanY - beta * meanX;

		res[i] = beta * x + alpha;
	}

	return res;
}*/



function flatten(arr) {
  return arr.reduce(function (flat, toFlatten) {
    return flat.concat(Array.isArray(toFlatten) ? flatten(toFlatten) : toFlatten);
  }, []);
} // flatten([[1, 2, 3], [4, 5]]);




gsub = function(text1, text2, array){
  var result = [];
  for(var i=0;i<array.length;i++){
    result.push(array[i].replace(text1,text2))
  }
  return(result)
}




intensity_for_each_sample_plot = function(dta){
             var sample_label = Object.keys(dta[0])
             sample_label.shift()
             var colors_index = Array.apply(null, {length: sample_label.length}).map(Number.call, Number)
             var colors_perc = colors_index.map(x => x/sample_label.length*100*3.6)
             var rainbow_colors = colors_perc.map(x => tinycolor("hsv("+x+" 100% 100%)"))
             var reds = unpack(rainbow_colors, "_r")
             var greens = unpack(rainbow_colors, "_g")
             var blues = unpack(rainbow_colors, "_b")
             var alphas = unpack(rainbow_colors, "_a")

             var data = []
             for(var i=0; i<sample_label.length;i++){
               var y = unpack(dta,sample_label[i])
               data.push({
                 x:Array(sample_label.length).fill(i),
                 y:y,
                 hoverinfo:"text",
                 text:"sample label: "+sample_label[i]+"<br>mean: "+jStat.mean(y)+"<br>median: "+jStat.median(y)+"<br>stdev: "+jStat.stdev(y),
                 type:"box",
                 fillcolor:"rgba("+reds[i]+","+greens[i]+","+blues[i]+","+alphas[i]+")",
                 marker:{
                   outliercolor:"rgba(0,0,0,1)",
                   line:{
                     width:1.5,
                     color:"rgba(0,0,0,1)"
                   },
                   size:5
                 },
                line:{
                  color:"rgba(51,51,51,1)",
                  width:1.5
                },
                name:sample_label[i],
                showlegend:false,
                xaxis:"x",
                yaxis:"y"
               })
             }
             var layout = {
               /*margin:{
                 t:40.27294,
                 r:17.305936,
                 b:64.22727,
                 l:47.26027
               },*/
               plot_bgcolor:"rgba(255,255,255,1)",
               paper_bgcolor:"rgba(255,255,255,1)",
               font:{
                 color:"rgba(0,0,0,1)",
                 family:"Dorid Sans",
                 size:15
               },
               xaxis:{
                 type:"linear",
                 autorange:true,
                 ticktext:sample_label,
                 tickvals:Array.apply(null, {length: sample_label.length}).map(Number.call, Number),
                 categoryorder:"array",
                 categoryarray:sample_label,
                 tickcolor:"rgba(0,0,0,1)",
                 tickfont:{
                   color:"rgba(0,0,0,1)",
                   family:"Dorid Sans",
                   size:12
                 },
                 tickangle:90,
                 showline:true,
                 showgrid:true,
                 anchor:"y",
                 title:"Sample Labels",
                 titlefont:{
                   family:"Dorid Sans",
                   size:15
                 }
               },
               yaxis:{
                 type:"log",
                 autorange:true,
                 tickcolor:"rgba(0,0,0,1)",
                 tickfont:{
                   color:"rgba(0,0,0,1)",
                   family:"Dorid Sans",
                   size:12
                 },
                 tickangle:90,
                 showline:true,
                 showgrid:true,
                 anchor:"x",
                 title:"Log (Intensity)",
                 titlefont:{
                   family:"Dorid Sans",
                   size:15
                 }
               },
               shapes:[
                 {
                   type:"rect",
                   fillcolor:"transparent",
                   line:{
                     color:"rgba(0,0,0,1)",
                     width:1,
                     linetype:"solid"
                   },
                     yref:"paper",
                     xref:"paper",
                     x0:0,x1:1,y0:0,y1:1
                 }
               ],
               showlegend:false,
               hovermode:"closest",
               barmode:"relative",
               titlefont:{
                 family:"Dorid Sans",
                 size:20,
                 color:"rgba(0,0,0,1)"
               }
             }

             return({data:data, layout:layout})
           }







vip_plot = function(vip, y_text,vip_heatmap_z,vip_heatmap_text, n_vip, height=undefined,left_margin=undefined,yaxis_font_size=10){
  var vip = vip.slice(1, n_vip+1);
  var y_text = y_text.slice(1, n_vip+1);
  var vip_heatmap_z = vip_heatmap_z.slice(1, n_vip+1);


  var y = Array.apply(null, {length: vip.length}).map(Number.call, Number)
  y = y.map(x=>x+1)

  vip_heatmap_x = Array.apply(null, {length: vip_heatmap_z[0].length}).map(Number.call, Number)
  vip_heatmap_x = vip_heatmap_x.map(x=>x+1)

  var trace1 ={
    x:vip,
    y:y,
    type:"scatter",
    xaxis:"x",
    yaxis:"y",
    mode: 'markers',
    marker:{
      size:12,
      color:"rgba(0,0,0,1)"
    }
  }
  var trace2 = {
    x:vip_heatmap_x,
    y:y,
    z:vip_heatmap_z,
    xgap:3,
    ygap:3,
    colorscale:"Viridis",
    type:'heatmap',
    showscale:true,
    colorbar:{
      thicknessmode:"fraction",
      thickness:0.01,
      lenmode:"fraction",
      len:0.3,
      outlinecolor:"white",
      nticks:2,
      ticklen:0,
      tickvals:[1,jStat.max(vip_heatmap_z[0])],
      ticktext:["low","high"],
      tickcolor:"black",
      tickfont:{
        family:"Dorid Sans",
        color:"black"
      }
    },
    showlegend:false,
    xaxis:"x2",
    yaxis:"y",
    hoverinfo:"skip",
    name:""
  }
  var layout = {
    height:height,
    margin:{
      t:31.03035,
      //r:7.305936,
      //b:82.23073,
      l:left_margin
    },
    font:{
      color:"rgba(0,0,0,1)",
      family:"Dorid Sans",
      size:15
    },
    xaxis:{
      type:"linear",
      domain:[0,0.89],
      ticklen:3.5,
      showticklabels:true,
      tickfont:{
        color:"rgba(0,0,0,1)",
        family:"Dorid Sans",
        size:16
      },
      tickangle:-45,
      showline:false,
      showgrid:false,
      anchor:"y"
    },
    yaxis:{
      type:"linear",
      autorange:"reversed",
      range:[0,jStat.max(y)],
      showgrid:true,
      gridcolor:"rgba(157,157,157,1)",
      tickvals:y,
      ticktext:y_text,
      tickfont:{
        color:"rgba(0,0,0,1)",
        family:"Dorid Sans",
        size:yaxis_font_size
      }
    },
    xaxis2:{
      type:"linear",
      anchor:"y",
      domain:[0.91,1],
      showlines:false,
      showgrid:true,
      anchor:"y",
      tickvals:vip_heatmap_x,
      ticktext:vip_heatmap_text
    },
    showlegend:false,
    hovermode:"closest",
    barmode:"relative",
    shapes:[{
     type:"rect",
     fillcolor:"transparent",
     line:{
       color:"rgba(0,0,0,1)",
       width:2,
       linetype:"solid"
     },
     yref:"paper",
     xref:"paper",
     x0:0,
     x1:0.89,
     y0:0,
     y1:1
   }],
    title:"<b>Top "+n_vip+" VIP Score Plot</b>"
  }
  return({data:[trace1,trace2],layout:layout})

}





perm_plot = function(perm){
                     var sim = unpack(perm,'sim')
                     var y = unpack(perm,"y")
                     var type = unpack(perm, "type")
                     var R2_index = getAllIndexes(type,"R2")
                     var Q2_index = getAllIndexes(type,"Q2")
                     var R2_sim = R2_index.map(x => sim[x])
                     var Q2_sim = Q2_index.map(x => sim[x])
                     var R2_y = R2_index.map(x=> y[x])
                     var Q2_y = Q2_index.map(x=> y[x])

                     var sim_mean = jStat.mean(R2_sim.slice(1))
                     var R2_mean = jStat.mean(R2_y.slice(1))
                     var Q2_mean = jStat.mean(Q2_y.slice(1))
                     var R2_slope = (R2_y[0] - R2_mean)/(1-sim_mean)
                     var Q2_slope = (Q2_y[0] - Q2_mean)/(1-sim_mean)
                     var R2_intercept = R2_y[0] - R2_slope
                     var Q2_intercept = Q2_y[0] - Q2_slope



                     var trace_Q2 = {
                       x:Q2_sim.slice(1),
                       y:Q2_y.slice(1),
                       type:"scatter",
                       mode:"markers",
                       marker:{
                         color:"rgba(218,97,86,1)",
                         size:6,
                         symbol:"circle-open"
                       },
                       hoveron:"points",
                       name:"Q2",
                       legendgroup:"Q2",
                       showlegend:false,
                       xaxis:"x",
                       yaxis:"y",
                       hoverinfo:"y"
                     }
                     var trace_Q2_dot = {
                       x:[Q2_sim[0]],
                       y:[Q2_y[0]],
                       type:"scatter",
                       mode:"markers",
                       marker:{
                         color:"rgba(218,97,86,1)",
                         size:12,
                         symbol:"circle"
                       },
                       hoveron:"points",
                       name:"Q2",
                       legendgroup:"Q2",
                       showlegend:true,
                       xaxis:"x",
                       yaxis:"y",
                       hoverinfo:"y"
                     }
                     var trace_Q2_line = {
                       x:[Q2_sim[0],0],
                       y:[Q2_y[0], Q2_intercept],
                       type:"scatter",
                       mode:"lines",
                       line:{
                         color:"rgba(218,97,86,1)",
                         dash:"dash"
                       },
                       name:"Q2_line",
                       legendgroup:"Q2",
                       showlegend:false,
                       xaxis:"x",
                       yaxis:"y",
                       hoverinfo:"skip"
                     }
                     var trace_R2 = {
                       x:R2_sim.slice(1),
                       y:R2_y.slice(1),
                       type:"scatter",
                       mode:"markers",
                       marker:{
                         color:"rgba(112,177,192,1)",
                         size:6,
                         symbol:"circle-open"
                       },
                       hoveron:"points",
                       name:"R2",
                       legendgroup:"R2",
                       showlegend:false,
                       xaxis:"x",
                       yaxis:"y",
                       hoverinfo:"y"
                     }
                     var trace_R2_dot = {
                       x:[R2_sim[0]],
                       y:[R2_y[0]],
                       type:"scatter",
                       mode:"markers",
                       marker:{
                         color:"rgba(112,177,192,1)",
                         size:12,
                         symbol:"circle"
                       },
                       hoveron:"points",
                       name:"R2",
                       legendgroup:"R2",
                       showlegend:true,
                       xaxis:"x",
                       yaxis:"y",
                       hoverinfo:"y"
                     }
                     var trace_R2_line = {
                       x:[R2_sim[0],0],
                       y:[R2_y[0],R2_intercept],
                       type:"scatter",
                       mode:"lines",
                       line:{
                         color:"rgba(112,177,192,1)",
                         dash:"dash"
                       },
                       name:"R2_line",
                       legendgroup:"R2",
                       showlegend:false,
                       xaxis:"x",
                       yaxis:"y",
                       hoverinfo:"skip"
                     }
                     var layout={
                       margin:{
                         t:35
                       },
                       font:{
                         color:'black',
                         family:"Dorid Sans",
                         size:15
                       },
                       xaxis:{
                         type:"linear",
                         autorange:true,
                         tickcolor:"black",
                         tickfont:{
                           color:"black",
                           family:"Dorid Sans",
                           size:10
                         },
                         showline:false,
                         showgrid:true,
                         gridcolor:"rgba(235,235,235,1)",
                         title:"<b>Correlation Coefficient Between Permutated Data and Original Data</b>",
                         titlefont:{
                           color:"black",
                           family:"Dorid Sans",
                           size:15
                         }
                       },
                       yaxis:{
                         type:"linear",
                         autorange:true,
                         tickcolor:"black",
                         tickfont:{
                           color:"black",
                           family:"Dorid Sans",
                           size:10
                         },
                         showline:false,
                         showgrid:true,
                         gridcolor:"rgba(235,235,235,1)",
                         title:"<b>R2 and Q2</b>",
                         titlefont:{
                           color:"black",
                           family:"Dorid Sans",
                           size:15
                         }
                       },
                       shapes:[
                         {
                           type:"rect",
                           fillcolor:"transparent",
                           line:{
                             color:"rgba(0,0,0,1)",
                             width:1,
                             dash:"solid"
                           },
                           yref:"paper",
                           xref:"paper",
                           x0:0,x1:1,y0:0,y1:1
                         }
                       ],
                       title:"<b>Permutation Result</b>",
                       titlefont:{
                         color:"black",
                         family:"Dorid Sans",
                         size:20
                       }
                     }
                     return({data:[trace_Q2,trace_Q2_dot,trace_Q2_line,trace_R2,trace_R2_dot,trace_R2_line], layout:layout})
                 }
