d3.json("/data.json", function(data) {
      var cluster = new Set ();
      data.forEach(function(element){
        cluster.add(element.category);
        });
      var clusterarray = [];
      var n = data.length, // total number of nodes
          m = cluster.size; // number of distinct cluster


      var colorMap = {};
      var i = 0;
      cluster.forEach(function(cate) {
        clusterarray.push(cate);
        colorMap[cate] = i ++;
      });

      var width = 960,
          height = 500,
          padding = 1.5, // separation between same-color nodes
          clusterPadding = 6, // separation between different-color nodes
          maxRadius = 8;

      var color = d3.schemeCategory20;
////////////////////// data processing
function generatetags(tags){

  var currenttags = [];
  var cummulate = 0;
  tags.forEach(function(element){
        currenttags = currenttags.concat(Object.keys(element));
  })
  var result =
    currenttags.reduce(function(obj, k) {
    // define object property, treat as 0 if not defined
    tags.forEach(function(element){
                    //console.log(k);
                    cummulate += (element[k] || 0)

                    return cummulate;
                 });
    obj[k] = cummulate;
    cummulate = 0;
    return obj;
    // set initial value as an empty object
  }, {});
    return result;
}
///////////////////////sort array obj get largest 7
function sortProperties(obj)
{
  // convert object into array
	var sortable=[];
	for(var key in obj)
		if(obj.hasOwnProperty(key))
			sortable.push([key, obj[key]]); // each item is an array in format [key, value]

	// sort items by value
	sortable.sort(function(a, b)
	{
	  return b[1]-a[1]; // compare numbers
	});
  var returnsortable = [];
  for (i = 0; i < 6; i++) {
    returnsortable.push(sortable[i]);
  }
  var result = []
  returnsortable.forEach(function(d) {
    result.push({
         "label": d[0],
         "value": d[1]
    })
  });
//  console.log(result);
	return result; // array in format [ [ key1, val1 ], [ key2, val2 ], ... ]
}
/////////////////////////



      // The largest node for each cluster.
      var clusters = new Array(m);
      var pietags = new Array();

      function radius_adjuster(radius){
        if(radius >= 35)
        radius = 35;
        return radius;
      }

      var libraryid = 0;
      var nodes = data.map(function (element) {
        //generate tags attribute
        var tags = [];
        for (var key in element.month){
                  tags.push(element.month[key].tags);
        }
        var t = generatetags(tags);
        //console.log(sortProperties(t));
        pietags.push(sortProperties(t));
        //generate attributes
        var i = colorMap[element.category],
            r = radius_adjuster(element.count/20),
            //r = Math.sqrt((i + 1) / m * -Math.log(Math.random())) * maxRadius,
            d = {
              cluster: i,
              radius: r,
              tags: t,
              libraryid: libraryid++,
              categories: element.category,
              //tags:
              x: Math.cos(i / m * 2 * Math.PI) * 150 + width / 2 + Math.random(),
              y: Math.sin(i / m * 2 * Math.PI) * 150 + height / 2 + Math.random()
            };

            //console.log(element.month);
        if (!clusters[i] || (r > clusters[i].radius)) clusters[i] = d;
        return d;
      });

      var simulation = d3.forceSimulation()
        // keep entire simulation balanced around screen center
        .force('center', d3.forceCenter(width/2, height/2))

        // cluster by section
        .force('cluster', d3.forceCluster()
          .centers(function (d) { return clusters[d.cluster]; })
          .strength(0.5))

        // apply collision with padding
        .force('collide', d3.forceCollide(function (d) { return d.radius + padding; }))

        .on('tick', layoutTick)
        .nodes(nodes);

      var svg = d3.select('body')
          .append('svg')
          .attr('width', width)
          .attr('height', height);

      svg.append('text')
            .attr('x',20)
            .attr('y',50)
            .attr('id',"title")
            .style("font-size", "50px")
            .text("hi");

    //D3 Vertical Legend//////////////////////////
      var legend3 = svg.selectAll('.legend3')
          .data(clusterarray)
          .enter().append('g')
          .attr("class", "legends3")
          .attr("transform", function (d, i) {
          {
              return "translate(0," + i * 20 + ")"
          }
      })

      legend3.append('rect')
          .attr("x", 0)
          .attr("y", 60)
          .attr("width", 10)
          .attr("height", 10)
          .style("fill", function (d, i) {
          return color[colorMap[d]];
      })

      legend3.append('text')
          .attr("x", 20)
          .attr("y", 70)
      //.attr("dy", ".35em")
      .text(function (d, i) {
          return d;
      })
          .attr("class", "textselected")
          .style("text-anchor", "start")
          .style("font-size", 15)

          ////////

      var node = svg.selectAll('circle')
        .data(nodes)
        .enter().append('circle')
          .attr('libraryid', function(d){
            return d.libraryid;
          })
          .attr('categories', function (d){
            return d.categories;})
          .style('fill', function (d) {
            return color[colorMap[d.categories]]; })
          .on("mouseover", handleMouseOver)
          .on("mouseout", handleMouseOut)
          //.on("click", change)
          .on("click", handleMouseClick);

          // Create Event Handlers for mouse
      function handleMouseClick(){
            var radius = d3.select(this).attr('r');
            d3.select(this) .transition()
                            .duration(50)
                            .delay(50)
                            .attr('r', radius * 0.8)
                            .transition()
                            .delay(50)
                            .duration(50)
                            .attr('r', radius);

            //console.log("d3.select(this).attr('tags'): " + d3.select(this).attr('tags'));
            //console.log(pietags[d3.select(this).attr('libraryid')]);
            //console.log(makeData());
            var s = update(pietags[d3.select(this).attr('libraryid')]);
            //console.log(s);
        }
      function handleMouseOver(d, i) {  // Add interactivity
            // Use D3 to select element, change color and size
            var color = d3.select(this).style("fill");
            var category = d3.select(this).attr("categories");
            d3.select(this).style("fill", function() {
                return d3.rgb(getComputedStyle(this, null).getPropertyValue("fill")).brighter();
              });
            d3.select("#title").style("fill",color)
                               .text(category);

          }

      function handleMouseOut(d, i) {
            // Use D3 to select element, change color back to normal
            d3.select(this).style("fill", function() {
                return d3.rgb(getComputedStyle(this, null).getPropertyValue("fill")).darker();
              });

          }


      function layoutTick (e) {
        node
          .attr('cx', function (d) { return d.x; })
          .attr('cy', function (d) { return d.y; })
          .attr('r', function (d) { return d.radius; });
      }

});
