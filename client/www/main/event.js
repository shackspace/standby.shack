/******************************************************
 * author: Mr. Pi <mrpi@mr-pi.de>
 * copyright: 2013 Mr. Pi
 *-----------------------------------------------------
 * all event driven stuff
 * ****************************************************/

function setupEventSource() {
	var source = new EventSource('/eventsource');

	source.addEventListener('message', function(event) {
		var obj = JSON.parse(event.data);
		if(obj.type === "power") {
			updateChart(obj.p1,obj.p2,obj.p3);
		}
	}, false);
}

function showLightControl() {
	document.getElementById("lightcontrol").style.opacity = "1.0";
	document.getElementById("lightcontrol").style.zindex = 10;
}

function showPowerMon() {
	document.getElementById("lightcontrol").style.opacity = "0.1";
	document.getElementById("lightcontrol").style.zindex = 8;
}
