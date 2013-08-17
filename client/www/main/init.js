/******************************************************
 * author: Mr. Pi <mrpi@mr-pi.de>
 * copyright: 2013 Mr. Pi
 *-----------------------------------------------------
 * 
 * ****************************************************/

window.onload = function() {
	if (!!window.EventSource) {
		setupEventSource();
	} else {
		alert("Sorry but your browser doesn't support the EventSource API");
	}
	alert(document.getElementById('powerLogNum').value);
	initPowerChart("powermon", document.getElementById('powerLogNum').value);
	initLights();
	lightResize();
	if(window.location.hash == "#powermon") {
		showPowerMon();
	}
}
window.onresize = function(event) {
	lightResize();
}
