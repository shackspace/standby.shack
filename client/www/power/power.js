dataP1 = [];
dataP2 = [];
dataP3 = [];
dataSum = [];

function chartinit() {
	energieChart = new CanvasJS.Chart("energie",{
		zoomEnabled: true,
		panEnabled: true,
		toolTip: {
			shared: true,
		},
		title: {
			text: "energie usage:",
			horizontalAlign: "left",
			fontSize: 14,
			fontWeight: "bold",
			fontFamily: "calibri"
		},
		legend: {
			horizontalAlign: "left",
			fontSize: 14,
			fontWeight: "bold",
			fontFamily: "calibri",
			fontColor: "dimGrey",
			valueFormatString: "##0 000"
		},
		axisY2:{
			suffix: 'W',
			includeZero: false,
			valueFormatString: "##0 000"
		},
		axisX:{      
			valueFormatString: "HH:mm:ss"
		},
		data: [
			{ 
				type: "line",
				axisYType: "secondary",
				xValueType: "dateTime",
				showInLegend: true,
				name: "P1",
				dataPoints: dataP1
			}, {				
				type: "line",
				axisYType: "secondary",
				xValueType: "dateTime",
				showInLegend: true,
				name: "P2" ,
				dataPoints: dataP2
			}, {				
				type: "line",
				axisYType: "secondary",
				xValueType: "dateTime",
				showInLegend: true,
				name: "P3" ,
				dataPoints: dataP3
			}, {
				type: "line",
				axisYType: "secondary",
				xValueType: "dateTime",
				showInLegend: true,
				name: "Sum",
				dataPoints: dataSum
			}
		]
	});

	var time = new Date();
	time.setTime(time.getTime()-1000*300);

	for (var i = 0; i < 500; i++) {
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
		dataSum.push({
			x: time.getTime(),
			y: 1000
		});
	};

	energieChart.options.data[0].legendText = " P1: " + -1 + "W ";
	energieChart.options.data[1].legendText = " P2: " + -1 + "W ";
	energieChart.options.data[2].legendText = " P3: " + -1 + "W ";
	energieChart.options.data[3].legendText = " Sum: " + -1 + "W ";

	energieChart.render();
}
function updateChart(p1,p2,p3) {
	var time = new Date();
	var sum = p1 + p2 + p3;
	
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
	dataSum.push({
		x: time.getTime(),
		y: sum
	});
	dataSum.shift();

	energieChart.options.data[0].legendText = " P1: " + p1 + "W ";
	energieChart.options.data[1].legendText = " P2: " + p2 + "W ";
	energieChart.options.data[2].legendText = " P3: " + p3 + "W ";
	energieChart.options.data[4].legendText = " Sum: " + sum + "W ";

	energieChart.render();
}
