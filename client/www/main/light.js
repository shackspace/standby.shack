/******************************************************
 * author: Mr. Pi <mrpi@mr-pi.de>
 * copyright: 2013 Mr. Pi
 *-----------------------------------------------------
 * lightcontrol
 * ****************************************************/

curL=0, curR=0, curO=0;
lights = new Array();

function showBgLight(inL,inR,inO) {
	document.getElementById("lbg000").style.zIndex = 100;
	document.getElementById("lbg001").style.zIndex = 100;
	document.getElementById("lbg010").style.zIndex = 100;
	document.getElementById("lbg011").style.zIndex = 100;
	document.getElementById("lbg100").style.zIndex = 100;
	document.getElementById("lbg101").style.zIndex = 100;
	document.getElementById("lbg110").style.zIndex = 100;
	document.getElementById("lbg111").style.zIndex = 100;
	document.getElementById("lbg"+inO+inR+inL).style.zIndex = 101;
}

function setLight(id, state) {
	alert("set light: " + id + " to " + state);
	showBgLight(curL, curR, curO);
}

function previewLight(id, state) {
	if(state === 1) {
		showBgLight(
			(curL | lights[id].grL),
			(curR | lights[id].grR),
			(curO | lights[id].grO)
		);
	} else {
		showBgLight(
			(curL & ((lights[id].grL === 0)?1:0)),
			(curR & ((lights[id].grR === 0)?1:0)),
			(curO & ((lights[id].grO === 0)?1:0))
		);
	}
}

function initLights() {
	showBgLight(0,0,0);
	initLight(1,156,75,160,70,10,0,0,1);
	initLight(2,347,27,143,61,11,0,0,1);
	initLight(3,204,139,174,84,12,0,0,1);
	initLight(4,407,81,155,72,13,0,0,1);
	initLight(5,331,234,71,53,18,1,1,1);
	initLight(6,535,162,66,46,19,1,1,1);
	initLight(7,49,417,249,156,14,1,0,0);
	initLight(8,352,308,208,124,15,0,1,0);
	initLight(9,586,221,180,101,16,0,1,0);
}

function initLight(idIntern, xPos, yPos, width, height, idAPI, inL, inR, inO) {
	var fStrGr = inL + "," + inR + "," + inO;

	document.getElementById("lightcontainer").innerHTML = 
		document.getElementById("lightcontainer").innerHTML +
		"<img src=\"./ciko/"+ idIntern +"-green.png\"" +
			" id=\"llid"+ idAPI +"on\"" + 
			" onclick=\"setLight("+ idAPI +", 0);\"" +
			" onmouseover=\"previewLight("+ idAPI +", 0);\"" +
			" onMouseout=\"showBgLight(curL,curR,curO);\"" +
		" />" +
		"<img src=\"./ciko/"+ idIntern +"-red.png\"" + 
			" id=\"llid"+ idAPI +"off\" " + 
			" onclick=\"setLight("+ idAPI +", 1);\"" +
			" onmouseover=\"previewLight("+ idAPI +", 1);\"" +
			" onMouseout=\"showBgLight(curL,curR,curO);\"" +
		" />";

	lights[idAPI] = {
		"grL": inL,
		"grR": inR,
		"grO": inO,
		"state": 0
	};

	document.getElementById("llid" + idAPI + "on").style.position = "absolute";
	document.getElementById("llid" + idAPI + "on").style.zIndex = 200;
	document.getElementById("llid" + idAPI + "on").style.marginTop = yPos + "px";
	document.getElementById("llid" + idAPI + "on").style.marginLeft = xPos + "px";
	document.getElementById("llid" + idAPI + "on").style.height = height + "px";
	document.getElementById("llid" + idAPI + "on").style.width = width + "px";

	document.getElementById("llid" + idAPI + "off").style.position = "absolute";
	document.getElementById("llid" + idAPI + "off").style.zIndex = 201;
	document.getElementById("llid" + idAPI + "off").style.marginTop = yPos + "px";
	document.getElementById("llid" + idAPI + "off").style.marginLeft = xPos + "px";
	document.getElementById("llid" + idAPI + "off").style.height = height + "px";
	document.getElementById("llid" + idAPI + "off").style.width = width + "px";
}
