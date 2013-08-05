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
			updatePowerChart(obj.p1,obj.p2,obj.p3);
		}
		else if(obj.type === "states") {
			updateLight(obj.address[1],obj.state);
		}
	}, false);
}

function showLightControl() {
	document.getElementById("lightcontrol").style.opacity = "1.0";
	document.getElementById("lightcontrol").style.zIndex = 12;
}

function showPowerMon() {
	document.getElementById("lightcontrol").style.opacity = "0.0";
	window.setTimeout(function() {
		document.getElementById("lightcontrol").style.zIndex = 8;
	}, 500);
}
function lightResize() {
	document.getElementById("lightcontainer").style.zoom =
		document.getElementById("lightcontrol").clientHeight /
		document.getElementById("lightcontainer").clientHeight;
}
