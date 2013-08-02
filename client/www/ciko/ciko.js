require(["dojo/on", "dojo/dom", "dojo/dom-style", "dojo/mouse", "dojox/gfx", "dojo/request", "dojo/json", "dojo/request/xhr", "dojo/ready", "dojox/timing", "dojo/domReady!"], function(on, dom, domStyle, mouse, gfx, request, json, xhr, ready, timing){
	var surfaceShackspace = gfx.createSurface("surfaceShackspace", 800, 600);
	var lights = new Array();
	var serverEventConnection = new EventSource("http://standby.shack:8091/eventsource");
	t = new dojox.timing.Timer(1000);		//To update Background Image

	lights.push(new Light([1], [156], [75], [160], [70], surfaceShackspace, [121, 122, 125, 126], 10));
	lights.push(new Light([2], [347], [27], [143], [61], surfaceShackspace, [119, 120, 123, 124], 11));
	lights.push(new Light([3], [204], [139], [174], [84], surfaceShackspace, [117, 118], 12));
	lights.push(new Light([4], [407], [81], [155], [72], surfaceShackspace, [115, 166], 13));
	lights.push(new Light([5, 6], [331, 535], [234, 162], [71, 66], [53, 46], surfaceShackspace, [112, 113, 114], 17));
	lights.push(new Light([7], [49], [417], [249], [156], surfaceShackspace, [108, 109, 110, 111], 14));
	lights.push(new Light([8], [352], [308], [208], [124], surfaceShackspace, [104, 105, 106, 107], 15));
	lights.push(new Light([9], [586], [221], [180], [101], surfaceShackspace, [100, 101, 102, 103], 16));

	var shackLogo = surfaceShackspace.createImage({x:620, y:500, width: 180, height:80, src:"logo_shack_large.png"});
	//var background = surfaceShackspace.createImage({x:0, y:0, width: 1024, height:768, src:"Background.png"});
	var backgroundImage = surfaceShackspace.createImage({x:0, y:0, width: 800, height:600, src:"shack_bare.png"});
	update();

	t.onTick = function(){
		updateBackground();
		console.log("Tick");
		t.stop(); 
	}

	var clockOverlay = function(){

	}

	function updateBackground(){
		backName = ".png";
		if(lights[5].getState() == "on"){
			backName = "1" + backName;
		}
		else{
			backName = "0" + backName;
		}

		if(lights[4].getState() == "on" || lights[6].getState() == "on" || lights[7].getState() == "on"){
			backName = "1" + backName;
		}
		else{
			backName = "0" + backName;
		}

		if(lights[0].getState() == "on" ||
		   lights[1].getState() == "on" ||
		   lights[2].getState() == "on" ||
		   lights[3].getState() == "on") {
			backName = "1" + backName;
		}
		else{ 
			backName = "0" + backName;
		}

		backName = "zone" + backName;

		backgroundImage.removeShape();
		backgroundImage.destroy();
		backgroundImage = surfaceShackspace.createImage({x:0, y:0, width: 800, height:600, src:backName});
		backgroundImage.moveToBack();
		shackLogo.moveToBack();
		//background.moveToBack();
	}

	function update(){
		for(abc in lights){
			lights[abc].redraw(surfaceShackspace);
			console.log("Updating Light with groupID: "
				    + lights[abc].groupID + " selected by key: " + abc);
			lights[abc].update();
		}
		t.start(); //Update Background Image after the calls have finished
	}

	serverEventConnection.addEventListener('message', function(e) {
		var data = JSON.parse(e.data);
		if(data.type == "states"){
			for(key in lights){
				var hwIDs = lights[key].getHwId()
				for(innerKey in hwIDs){
					if(hwIDs[innerKey] == data.id){
						if((data.state == 1 && lights[key].getState() == "on") ||
						   (data.state == 0 && lights[key].getState() == "off")) {
							//Lamp has already the correct state
						}
						else{
							if(data.state == 1) {
								lights[key].setState("on");
							}
							else if(data.state == 0) {
								lights[key].setState("off");
							}
							lights[key].redraw(surfaceShackspace);
							updateBackground();
						}
						break;
					}
				}
			}
		}
	}, false);

	function Light(positionIDs, xPos, yPos, width, height, surface, hwID, groupID){
		this.positionIDs = positionIDs;
		this.state = "off";
		this.surface = surface;
		this.xPos = xPos;
		this.yPos = yPos;
		this.height = height;
		this.width = width;
		this.visual = new Array();
		this.hwID = hwID;
		this.groupID = groupID;
		var _this = this;	//Its needed because dojo is crap in the request below

		for(key in positionIDs) {
			this.visual.push(surface.createImage({
				x: xPos[key].toString(),
				y: yPos[key].toString(),
				width: this.width[key],
				height: this.height[key],
				src: this.positionIDs[key].toString() + "-red.png"}));
		}

		this.switchOn = function() {
			xhr.put("http://standby.shack:8091/2/lounge/" + this.groupID, {
				headers: {"Content-Type": "application/json"},
				data: json.stringify({"type": "switchOn"})
			});
			this.state = "on";
		}

		this.switchOff = function() {
			xhr.put("http://standby.shack:8091/2/lounge/" + this.groupID, {
				headers: {"Content-Type": "application/json"},
				data: json.stringify({"type": "switchOff"})
			});
			this.state = "off";
		}

		this.toggle = function(){
			xhr.put("http://standby.shack:8091/2/lounge/" + this.groupID, {
				headers: {"Content-Type": "application/json"},
				data: json.stringify({"type": "toggle"})
			});
			if(this.state == "off"){
				this.state = "on";
				this.redraw(this.surface);
			}
			else{
				this.state = "off";
				this.redraw(this.surface);
			}
		}

		this.getState = function(){
			return this.state;
		}

		this.setState = function(state){
			this.state = state;
		}

		this.setPosition = function(positions){
			this.x = positions[0];
			this.y = positions[1];
		}

		this.getVisual = function(){
			return this.visual;
		}

		this.getHwId = function(){
			return this.hwID;
		}

		this.redraw = function(surface){
			for(key in this.positionIDs){
				this.visual[key].removeShape();
				this.visual[key].destroy();
				if(this.state=="off") {
					this.visual[key] = surface.createImage({
						x: xPos[key].toString(), 
						y: yPos[key].toString(),
						width: this.width[key],
						height: this.height[key],
						src: this.positionIDs[key].toString() + "-red.png"});
				}
				else{
					this.visual[key] = surface.createImage({
						x: xPos[key].toString(),
						y: yPos[key].toString(),
						width: this.width[key],
						height: this.height[key],
						src: this.positionIDs[key].toString() + "-green.png"});
				}
				this.visual[key].connect("onclick", function(e){
					console.log(e.gfxTarget);
					for(key in lights) {
						for(visualKey in lights[key].getVisual()) {
							if(lights[key].getVisual()[visualKey] == e.gfxTarget) {
								lights[key].toggle();
								_this.redraw(_this.surface);
								updateBackground();
								break;
							}
						}
					}
				});
			}
		}

		this.update = function(){
			console.log("Requesting: http://standby.shack:8091/2/lounge/" + this.groupID);
			request.get("http://standby.shack:8091/2/lounge/" + this.groupID, { headers:{"Content-Type": "application/json"} }).then(
				function(response){
					console.log(response);
					if(response != "{}"){
						lightResponse = JSON.parse(response);
						if(lightResponse.state == 1){
							_this.state = "on";
						}
						else{
							_this.state = "off";
						}
						_this.redraw(_this.surface);
					}
				}
			);
		}
	}
});
