dataP1 = [];
dataP2 = [];
dataP3 = [];

function chartinit() {
	energieChart = new CanvasJS.Chart("energie",{
		zoomEnabled: true,
		title: {
			text: "energie usage"		
		},
		toolTip: {
			shared: false,
		},
		legend: {
			verticalAlign: "top",
			horizontalAlign: "center",
			fontSize: 14,
			fontWeight: "bold",
			fontFamily: "calibri",
			fontColor: "dimGrey"
		},
		axisX: {
		},
		axisY:{
			suffix: 'W',
			includeZero: false
		}, 
		data: [{ 
			type: "line",
			xValueType: "dateTime",
			showInLegend: true,
			name: "P1",
			dataPoints: dataP1
		},
		{				
			type: "line",
			xValueType: "dateTime",
			showInLegend: true,
			name: "P2" ,
			dataPoints: dataP2
		},
		{				
			type: "line",
			xValueType: "dateTime",
			showInLegend: true,
			name: "P3" ,
			dataPoints: dataP3
		}]
	});

	var time = new Date();
	time.setTime(time.getTime()-1000*300);

	for (var i = 0; i < 300; i++) {
		time.setTime(time.getTime()+1000);
		dataP1.push({
			x: time.getTime(),
			y: 1000
		});
		dataP2.push({
			x: time.getTime(),
			y: 1000
		});
		dataP3.push({
			x: time.getTime(),
			y: 1000
		});
	};

	energieChart.options.data[0].legendText = " P1: " + -1 + "W";
	energieChart.options.data[1].legendText = " P2: " + -1 + "W";
	energieChart.options.data[2].legendText = " P3: " + -1 + "W";

	energieChart.render();
}
function updateChart(p1,p2,p3) {
	var time = new Date();
	
	dataP1.push({
		x: time.getTime(),
		y: p1
	});
	dataP1.shift();
	dataP2.push({
		x: time.getTime(),
		y: p2
	});
	dataP2.shift();
	dataP3.push({
		x: time.getTime(),
		y: p3
	});
	dataP3.shift();

	energieChart.options.data[0].legendText = " P1: " + p1 + "W";
	energieChart.options.data[1].legendText = " P2: " + p2 + "W";
	energieChart.options.data[2].legendText = " P3: " + p3 + "W";

	energieChart.render();
}