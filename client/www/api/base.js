/******************************************************
 * author: Mr. Pi <mrpi@mr-pi.de>
 * copyright: 2013 Mr. Pi
 *-----------------------------------------------------
 * base functions
 * ****************************************************/

function httpPut(theUrl,data) {
	var xmlhttp=new XMLHttpRequest();
	xmlhttp.open("PUT", theUrl, true);
	xmlhttp.setRequestHeader('Content-Type', 'application/json');
	xmlhttp.send(data);
}

function httpGet(theUrl) {
	var xmlhttp=new XMLHttpRequest();
	xmlhttp.open("GET", theUrl, false);
	xmlhttp.send(); 
	return xmlhttp.response;
}
