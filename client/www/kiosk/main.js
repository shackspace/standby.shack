/******************************************************
 * author: Mr. Pi <mrpi@mr-pi.de>
 * copyright: 2013 Mr. Pi
 *-----------------------------------------------------
 * Lightcontroll
 * ****************************************************/

function httpPut(theUrl,data) {
	var xmlhttp=new XMLHttpRequest();
	xmlhttp.open("PUT", theUrl, true);
	xmlhttp.setRequestHeader('Content-Type', 'text/plain');
	xmlhttp.send(data);
}

function httpGet(theUrl) {
	var xmlhttp=new XMLHttpRequest();
	xmlhttp.open("GET", theUrl, false);
	xmlhttp.send(); 
	return xmlhttp.response;
}

function setupEventSource() {
	var source = new EventSource('/eventsource');

	source.addEventListener('message', function(event) {
		var obj = JSON.parse(event.data);
		if(obj.type === "states") {
			updateLight(obj.id,obj.state);
		}
		else if(obj.type === "power") {
			updateChart(obj.p1,obj.p2,obj.p3);
		}
		}, false);
}

window.onload = function() {
	if (!!window.EventSource) {
		setupEventSource();
	} else {
		alert("Sorry but your browser doesn't support the EventSource API");
	}
	chartinit();
}
