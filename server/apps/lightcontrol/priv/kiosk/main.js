/******************************************************
 * author: Mr. Pi <mrpi@mr-pi.de>
 * copyright: 2013 Mr. Pi
 *-----------------------------------------------------
 * Lightcontroll
 * ****************************************************/

function getId(x,y) {
	for(var i in lights) {
		if(x>=lights[i].minx && x<=lights[i].maxx && y>=lights[i].miny && y<=lights[i].maxy)
			return lights[i].id;
	}
	return -1;
}

function toggleLight(id) {
	httpPut('/2/light/'+id,'t');
}

function mouseClick(e) {
	var x=-1;
	var y=-1;
	if (!e) var e = window.event;
	if (e.pageX || e.pageY) {
		x=e.pageX;
		y=e.pageY;
	}
	else if (e.clientX || e.clientY) {
		x=e.clientX;
		y=e.clientY;
	}
	var id=getId(x,y);
	if(id>-1)
		toggleLight(id);
//	document.getElementById('myhead').innerHTML='X='+x+' Y='+y+' ID='+id;
}

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

function updateLight(id, state) {
	if(state == 0)
		document.getElementById('l'+id).style.color='#ff0000';
	else
		document.getElementById('l'+id).style.color='#00ff00';
}

function setupEventSource() {
	var source = new EventSource('/eventsource');

	source.addEventListener('message', function(event) {
		var obj = JSON.parse(event.data);
		if(obj.type = "states")
			updateLight(obj.id,obj.state);
		}, false);
}

window.onload = function() {
	for(var i=100; i<=126; i++) {
		var res = httpGet('/2/light/'+i);
		var obj = JSON.parse(res);
		if(obj.state=="0")
			document.getElementById('l'+i).style.color='#f00';
		else
			document.getElementById('l'+i).style.color='#0f0';
	}
	self.setInterval(function() {
		var cd = new Date();
		document.getElementById('myhead').innerHTML=cd.getHours()+':'+cd.getMinutes()+':'+cd.getSeconds();
	}, 1000);
	
	if (!!window.EventSource) {
		setupEventSource();
	} else {
		alert("Sorry but your browser doesn't support the EventSource API");
	}
}
