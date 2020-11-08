{
	"patcher" : 	{
		"fileversion" : 1,
		"appversion" : 		{
			"major" : 8,
			"minor" : 1,
			"revision" : 8,
			"architecture" : "x64",
			"modernui" : 1
		}
,
		"classnamespace" : "box",
		"rect" : [ 276.0, 151.0, 835.0, 643.0 ],
		"bglocked" : 0,
		"openinpresentation" : 1,
		"default_fontsize" : 12.0,
		"default_fontface" : 0,
		"default_fontname" : "Arial",
		"gridonopen" : 1,
		"gridsize" : [ 15.0, 15.0 ],
		"gridsnaponopen" : 1,
		"objectsnaponopen" : 1,
		"statusbarvisible" : 2,
		"toolbarvisible" : 1,
		"lefttoolbarpinned" : 0,
		"toptoolbarpinned" : 0,
		"righttoolbarpinned" : 0,
		"bottomtoolbarpinned" : 0,
		"toolbars_unpinned_last_save" : 0,
		"tallnewobj" : 0,
		"boxanimatetime" : 200,
		"enablehscroll" : 1,
		"enablevscroll" : 1,
		"devicewidth" : 0.0,
		"description" : "",
		"digest" : "",
		"tags" : "",
		"style" : "",
		"subpatcher_template" : "",
		"assistshowspatchername" : 0,
		"boxes" : [ 			{
				"box" : 				{
					"id" : "obj-13",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 113.0, 54.0, 29.5, 22.0 ],
					"text" : "0"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-9",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "bang" ],
					"patching_rect" : [ 113.0, 14.0, 58.0, 22.0 ],
					"text" : "loadbang"
				}

			}
, 			{
				"box" : 				{
					"comment" : "Set tonic",
					"id" : "obj-1",
					"index" : 0,
					"maxclass" : "inlet",
					"numinlets" : 0,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 573.0, 50.0, 30.0, 30.0 ]
				}

			}
, 			{
				"box" : 				{
					"comment" : "Output current value",
					"id" : "obj-92",
					"index" : 0,
					"maxclass" : "inlet",
					"numinlets" : 0,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 288.0, 50.0, 30.0, 30.0 ]
				}

			}
, 			{
				"box" : 				{
					"comment" : "Value when changed",
					"id" : "obj-91",
					"index" : 0,
					"maxclass" : "outlet",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 498.0, 575.0, 30.0, 30.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-90",
					"maxclass" : "newobj",
					"numinlets" : 15,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patcher" : 					{
						"fileversion" : 1,
						"appversion" : 						{
							"major" : 8,
							"minor" : 1,
							"revision" : 8,
							"architecture" : "x64",
							"modernui" : 1
						}
,
						"classnamespace" : "box",
						"rect" : [ 196.0, 149.0, 640.0, 480.0 ],
						"bglocked" : 0,
						"openinpresentation" : 0,
						"default_fontsize" : 12.0,
						"default_fontface" : 0,
						"default_fontname" : "Arial",
						"gridonopen" : 1,
						"gridsize" : [ 15.0, 15.0 ],
						"gridsnaponopen" : 1,
						"objectsnaponopen" : 1,
						"statusbarvisible" : 2,
						"toolbarvisible" : 1,
						"lefttoolbarpinned" : 0,
						"toptoolbarpinned" : 0,
						"righttoolbarpinned" : 0,
						"bottomtoolbarpinned" : 0,
						"toolbars_unpinned_last_save" : 0,
						"tallnewobj" : 0,
						"boxanimatetime" : 200,
						"enablehscroll" : 1,
						"enablevscroll" : 1,
						"devicewidth" : 0.0,
						"description" : "",
						"digest" : "",
						"tags" : "",
						"style" : "",
						"subpatcher_template" : "",
						"assistshowspatchername" : 0,
						"boxes" : [ 							{
								"box" : 								{
									"id" : "obj-15",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 2,
									"outlettype" : [ "bang", "int" ],
									"patching_rect" : [ 570.0, 90.0, 29.5, 22.0 ],
									"text" : "t b i"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-14",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 2,
									"outlettype" : [ "bang", "int" ],
									"patching_rect" : [ 495.0, 90.0, 29.5, 22.0 ],
									"text" : "t b i"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-13",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 2,
									"outlettype" : [ "bang", "int" ],
									"patching_rect" : [ 420.0, 90.0, 29.5, 22.0 ],
									"text" : "t b i"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-12",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 2,
									"outlettype" : [ "bang", "int" ],
									"patching_rect" : [ 345.0, 90.0, 29.5, 22.0 ],
									"text" : "t b i"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-11",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 2,
									"outlettype" : [ "bang", "int" ],
									"patching_rect" : [ 270.0, 90.0, 29.5, 22.0 ],
									"text" : "t b i"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-10",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 2,
									"outlettype" : [ "bang", "int" ],
									"patching_rect" : [ 195.0, 90.0, 29.5, 22.0 ],
									"text" : "t b i"
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-7",
									"index" : 15,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "int" ],
									"patching_rect" : [ 570.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-6",
									"index" : 13,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "int" ],
									"patching_rect" : [ 495.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-5",
									"index" : 11,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "int" ],
									"patching_rect" : [ 420.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-4",
									"index" : 9,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "int" ],
									"patching_rect" : [ 345.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-3",
									"index" : 7,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "int" ],
									"patching_rect" : [ 270.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-2",
									"index" : 5,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "int" ],
									"patching_rect" : [ 195.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-1",
									"index" : 3,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "int" ],
									"patching_rect" : [ 120.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-79",
									"maxclass" : "message",
									"numinlets" : 2,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 30.0, 150.0, 35.0, 22.0 ],
									"text" : "bang"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-77",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 90.0, 90.0, 72.0, 22.0 ],
									"text" : "prepend set"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-75",
									"maxclass" : "newobj",
									"numinlets" : 7,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 225.0, 210.0, 82.0, 22.0 ],
									"text" : "pack i i i i i i i"
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"cool" : 1,
									"id" : "obj-81",
									"index" : 2,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 90.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-82",
									"index" : 1,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "bang" ],
									"patching_rect" : [ 30.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"cool" : 1,
									"id" : "obj-83",
									"index" : 4,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 165.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"cool" : 1,
									"id" : "obj-84",
									"index" : 6,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 240.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"cool" : 1,
									"id" : "obj-85",
									"index" : 8,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 315.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"cool" : 1,
									"id" : "obj-86",
									"index" : 10,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 390.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"cool" : 1,
									"id" : "obj-87",
									"index" : 12,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 465.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"cool" : 1,
									"id" : "obj-88",
									"index" : 14,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 540.0, 30.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-89",
									"index" : 1,
									"maxclass" : "outlet",
									"numinlets" : 1,
									"numoutlets" : 0,
									"patching_rect" : [ 225.0, 255.0, 30.0, 30.0 ]
								}

							}
 ],
						"lines" : [ 							{
								"patchline" : 								{
									"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
									"destination" : [ "obj-75", 0 ],
									"midpoints" : [ 129.5, 75.0, 129.0, 75.0, 129.0, 172.0, 234.5, 172.0 ],
									"source" : [ "obj-1", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
									"destination" : [ "obj-75", 1 ],
									"midpoints" : [ 215.0, 173.0, 245.0, 173.0 ],
									"source" : [ "obj-10", 1 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-79", 0 ],
									"midpoints" : [ 204.5, 131.0, 39.5, 131.0 ],
									"source" : [ "obj-10", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
									"destination" : [ "obj-75", 2 ],
									"midpoints" : [ 290.0, 172.0, 255.5, 172.0 ],
									"source" : [ "obj-11", 1 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-79", 0 ],
									"midpoints" : [ 279.5, 130.0, 39.5, 130.0 ],
									"source" : [ "obj-11", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
									"destination" : [ "obj-75", 3 ],
									"midpoints" : [ 365.0, 170.0, 266.0, 170.0 ],
									"source" : [ "obj-12", 1 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-79", 0 ],
									"midpoints" : [ 354.5, 130.0, 39.5, 130.0 ],
									"source" : [ "obj-12", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
									"destination" : [ "obj-75", 4 ],
									"midpoints" : [ 440.0, 170.0, 276.5, 170.0 ],
									"source" : [ "obj-13", 1 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-79", 0 ],
									"midpoints" : [ 429.5, 130.0, 39.5, 130.0 ],
									"source" : [ "obj-13", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
									"destination" : [ "obj-75", 5 ],
									"midpoints" : [ 515.0, 169.0, 287.0, 169.0 ],
									"source" : [ "obj-14", 1 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-79", 0 ],
									"midpoints" : [ 504.5, 130.0, 39.5, 130.0 ],
									"source" : [ "obj-14", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
									"destination" : [ "obj-75", 6 ],
									"midpoints" : [ 590.0, 169.0, 297.5, 169.0 ],
									"source" : [ "obj-15", 1 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-79", 0 ],
									"midpoints" : [ 579.5, 130.0, 39.5, 130.0 ],
									"source" : [ "obj-15", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
									"destination" : [ "obj-10", 0 ],
									"source" : [ "obj-2", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
									"destination" : [ "obj-11", 0 ],
									"source" : [ "obj-3", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
									"destination" : [ "obj-12", 0 ],
									"source" : [ "obj-4", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
									"destination" : [ "obj-13", 0 ],
									"source" : [ "obj-5", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
									"destination" : [ "obj-14", 0 ],
									"source" : [ "obj-6", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
									"destination" : [ "obj-15", 0 ],
									"source" : [ "obj-7", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-89", 0 ],
									"source" : [ "obj-75", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
									"destination" : [ "obj-75", 0 ],
									"midpoints" : [ 99.5, 148.0, 234.5, 148.0 ],
									"source" : [ "obj-77", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-75", 0 ],
									"midpoints" : [ 39.5, 192.0, 234.5, 192.0 ],
									"source" : [ "obj-79", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
									"destination" : [ "obj-77", 0 ],
									"source" : [ "obj-81", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-79", 0 ],
									"source" : [ "obj-82", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
									"destination" : [ "obj-75", 1 ],
									"midpoints" : [ 174.5, 148.0, 245.0, 148.0 ],
									"source" : [ "obj-83", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
									"destination" : [ "obj-75", 2 ],
									"source" : [ "obj-84", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
									"destination" : [ "obj-75", 3 ],
									"midpoints" : [ 324.5, 150.0, 266.0, 150.0 ],
									"source" : [ "obj-85", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
									"destination" : [ "obj-75", 4 ],
									"midpoints" : [ 399.5, 150.0, 276.5, 150.0 ],
									"source" : [ "obj-86", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
									"destination" : [ "obj-75", 5 ],
									"midpoints" : [ 474.5, 150.0, 287.0, 150.0 ],
									"source" : [ "obj-87", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
									"destination" : [ "obj-75", 6 ],
									"midpoints" : [ 549.5, 150.0, 297.5, 150.0 ],
									"source" : [ "obj-88", 0 ]
								}

							}
 ]
					}
,
					"patching_rect" : [ 378.0, 470.0, 166.0, 22.0 ],
					"saved_object_attributes" : 					{
						"description" : "",
						"digest" : "",
						"globalpatchername" : "",
						"tags" : ""
					}
,
					"text" : "p PackHotCold"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-70",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "bang", "int" ],
					"patching_rect" : [ 350.0, 86.0, 29.5, 22.0 ],
					"text" : "t b i"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-60",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "bang", "int" ],
					"patching_rect" : [ 573.0, 485.0, 29.5, 22.0 ],
					"text" : "t b i"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-57",
					"maxclass" : "newobj",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 498.0, 530.0, 174.0, 22.0 ],
					"text" : "vexpr $i1 + $i2 @scalarmode 1"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-41",
					"maxclass" : "live.dial",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "float" ],
					"parameter_enable" : 1,
					"patching_rect" : [ 573.0, 410.0, 41.0, 48.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 29.0, 52.0, 41.0, 48.0 ],
					"saved_attribute_attributes" : 					{
						"valueof" : 						{
							"parameter_enum" : [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ],
							"parameter_longname" : "live.dial",
							"parameter_mmax" : 11,
							"parameter_shortname" : "Tonic",
							"parameter_type" : 2,
							"parameter_unitstyle" : 9
						}

					}
,
					"varname" : "live.dial"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-39",
					"items" : [ "major", ",", "minor" ],
					"maxclass" : "umenu",
					"numinlets" : 1,
					"numoutlets" : 3,
					"outlettype" : [ "int", "", "" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 350.0, 50.0, 100.0, 22.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 12.0, 15.0, 75.0, 22.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-37",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "int", "int" ],
					"patcher" : 					{
						"fileversion" : 1,
						"appversion" : 						{
							"major" : 8,
							"minor" : 1,
							"revision" : 8,
							"architecture" : "x64",
							"modernui" : 1
						}
,
						"classnamespace" : "box",
						"rect" : [ 59.0, 104.0, 640.0, 480.0 ],
						"bglocked" : 0,
						"openinpresentation" : 0,
						"default_fontsize" : 12.0,
						"default_fontface" : 0,
						"default_fontname" : "Arial",
						"gridonopen" : 1,
						"gridsize" : [ 15.0, 15.0 ],
						"gridsnaponopen" : 1,
						"objectsnaponopen" : 1,
						"statusbarvisible" : 2,
						"toolbarvisible" : 1,
						"lefttoolbarpinned" : 0,
						"toptoolbarpinned" : 0,
						"righttoolbarpinned" : 0,
						"bottomtoolbarpinned" : 0,
						"toolbars_unpinned_last_save" : 0,
						"tallnewobj" : 0,
						"boxanimatetime" : 200,
						"enablehscroll" : 1,
						"enablevscroll" : 1,
						"devicewidth" : 0.0,
						"description" : "",
						"digest" : "",
						"tags" : "",
						"style" : "",
						"subpatcher_template" : "",
						"assistshowspatchername" : 0,
						"boxes" : [ 							{
								"box" : 								{
									"id" : "obj-31",
									"maxclass" : "newobj",
									"numinlets" : 2,
									"numoutlets" : 1,
									"outlettype" : [ "int" ],
									"patching_rect" : [ 65.0, 265.0, 29.5, 22.0 ],
									"text" : "+ 1"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-28",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 2,
									"outlettype" : [ "int", "bang" ],
									"patching_rect" : [ 50.0, 175.0, 29.5, 22.0 ],
									"text" : "t i b"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-26",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 2,
									"outlettype" : [ "", "bang" ],
									"patching_rect" : [ 80.0, 100.0, 29.5, 22.0 ],
									"text" : "t l b"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-25",
									"maxclass" : "newobj",
									"numinlets" : 5,
									"numoutlets" : 4,
									"outlettype" : [ "int", "", "", "int" ],
									"patching_rect" : [ 65.0, 220.0, 61.0, 22.0 ],
									"text" : "counter"
								}

							}
, 							{
								"box" : 								{
									"id" : "obj-20",
									"maxclass" : "newobj",
									"numinlets" : 1,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 50.0, 145.0, 25.0, 22.0 ],
									"text" : "iter"
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-34",
									"index" : 1,
									"maxclass" : "inlet",
									"numinlets" : 0,
									"numoutlets" : 1,
									"outlettype" : [ "" ],
									"patching_rect" : [ 80.0, 40.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-35",
									"index" : 1,
									"maxclass" : "outlet",
									"numinlets" : 1,
									"numoutlets" : 0,
									"patching_rect" : [ 50.0, 347.0, 30.0, 30.0 ]
								}

							}
, 							{
								"box" : 								{
									"comment" : "",
									"id" : "obj-36",
									"index" : 2,
									"maxclass" : "outlet",
									"numinlets" : 1,
									"numoutlets" : 0,
									"patching_rect" : [ 85.0, 347.0, 30.0, 30.0 ]
								}

							}
 ],
						"lines" : [ 							{
								"patchline" : 								{
									"destination" : [ "obj-28", 0 ],
									"source" : [ "obj-20", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-31", 0 ],
									"source" : [ "obj-25", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-20", 0 ],
									"source" : [ "obj-26", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-25", 2 ],
									"source" : [ "obj-26", 1 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-25", 0 ],
									"source" : [ "obj-28", 1 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-35", 0 ],
									"source" : [ "obj-28", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-36", 0 ],
									"source" : [ "obj-31", 0 ]
								}

							}
, 							{
								"patchline" : 								{
									"destination" : [ "obj-26", 0 ],
									"source" : [ "obj-34", 0 ]
								}

							}
 ]
					}
,
					"patching_rect" : [ 378.0, 155.0, 49.0, 22.0 ],
					"saved_object_attributes" : 					{
						"description" : "",
						"digest" : "",
						"globalpatchername" : "",
						"tags" : ""
					}
,
					"text" : "p setlist"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-21",
					"maxclass" : "newobj",
					"numinlets" : 2,
					"numoutlets" : 7,
					"outlettype" : [ "", "", "", "", "", "", "" ],
					"patching_rect" : [ 408.0, 200.0, 82.0, 22.0 ],
					"text" : "gate 7"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-11",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 4,
					"outlettype" : [ "", "", "", "" ],
					"patching_rect" : [ 378.0, 125.0, 176.0, 22.0 ],
					"saved_object_attributes" : 					{
						"embed" : 0,
						"precision" : 6
					}
,
					"text" : "coll edv.scaleselector-scales.txt"
				}

			}
, 			{
				"box" : 				{
					"bgmode" : 0,
					"border" : 0,
					"clickthrough" : 0,
					"enablehscroll" : 0,
					"enablevscroll" : 0,
					"id" : "obj-8",
					"lockeddragscroll" : 0,
					"maxclass" : "bpatcher",
					"name" : "edv.noteselector.maxpat",
					"numinlets" : 1,
					"numoutlets" : 1,
					"offset" : [ 0.0, 0.0 ],
					"outlettype" : [ "int" ],
					"patching_rect" : [ 303.0, 290.0, 15.0, 126.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 160.0, 0.0, 10.0, 120.0 ],
					"viewvisibility" : 1
				}

			}
, 			{
				"box" : 				{
					"bgmode" : 0,
					"border" : 0,
					"clickthrough" : 0,
					"enablehscroll" : 0,
					"enablevscroll" : 0,
					"id" : "obj-7",
					"lockeddragscroll" : 0,
					"maxclass" : "bpatcher",
					"name" : "edv.noteselector.maxpat",
					"numinlets" : 1,
					"numoutlets" : 1,
					"offset" : [ 0.0, 0.0 ],
					"outlettype" : [ "int" ],
					"patching_rect" : [ 258.0, 290.0, 15.0, 126.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 150.0, 0.0, 10.0, 120.0 ],
					"viewvisibility" : 1
				}

			}
, 			{
				"box" : 				{
					"bgmode" : 0,
					"border" : 0,
					"clickthrough" : 0,
					"enablehscroll" : 0,
					"enablevscroll" : 0,
					"id" : "obj-6",
					"lockeddragscroll" : 0,
					"maxclass" : "bpatcher",
					"name" : "edv.noteselector.maxpat",
					"numinlets" : 1,
					"numoutlets" : 1,
					"offset" : [ 0.0, 0.0 ],
					"outlettype" : [ "int" ],
					"patching_rect" : [ 213.0, 290.0, 15.0, 126.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 140.0, 0.0, 10.0, 120.0 ],
					"viewvisibility" : 1
				}

			}
, 			{
				"box" : 				{
					"bgmode" : 0,
					"border" : 0,
					"clickthrough" : 0,
					"enablehscroll" : 0,
					"enablevscroll" : 0,
					"id" : "obj-5",
					"lockeddragscroll" : 0,
					"maxclass" : "bpatcher",
					"name" : "edv.noteselector.maxpat",
					"numinlets" : 1,
					"numoutlets" : 1,
					"offset" : [ 0.0, 0.0 ],
					"outlettype" : [ "int" ],
					"patching_rect" : [ 168.0, 290.0, 15.0, 126.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 130.0, 0.0, 10.0, 120.0 ],
					"viewvisibility" : 1
				}

			}
, 			{
				"box" : 				{
					"bgmode" : 0,
					"border" : 0,
					"clickthrough" : 0,
					"enablehscroll" : 0,
					"enablevscroll" : 0,
					"id" : "obj-4",
					"lockeddragscroll" : 0,
					"maxclass" : "bpatcher",
					"name" : "edv.noteselector.maxpat",
					"numinlets" : 1,
					"numoutlets" : 1,
					"offset" : [ 0.0, 0.0 ],
					"outlettype" : [ "int" ],
					"patching_rect" : [ 123.0, 290.0, 15.0, 126.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 120.0, 0.0, 10.0, 120.0 ],
					"viewvisibility" : 1
				}

			}
, 			{
				"box" : 				{
					"bgmode" : 0,
					"border" : 0,
					"clickthrough" : 0,
					"enablehscroll" : 0,
					"enablevscroll" : 0,
					"id" : "obj-3",
					"lockeddragscroll" : 0,
					"maxclass" : "bpatcher",
					"name" : "edv.noteselector.maxpat",
					"numinlets" : 1,
					"numoutlets" : 1,
					"offset" : [ 0.0, 0.0 ],
					"outlettype" : [ "int" ],
					"patching_rect" : [ 78.0, 290.0, 15.0, 126.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 110.0, 0.0, 10.0, 120.0 ],
					"viewvisibility" : 1
				}

			}
, 			{
				"box" : 				{
					"bgmode" : 0,
					"border" : 0,
					"clickthrough" : 0,
					"enablehscroll" : 0,
					"enablevscroll" : 0,
					"id" : "obj-2",
					"lockeddragscroll" : 0,
					"maxclass" : "bpatcher",
					"name" : "edv.noteselector.maxpat",
					"numinlets" : 1,
					"numoutlets" : 1,
					"offset" : [ 0.0, 0.0 ],
					"outlettype" : [ "int" ],
					"patching_rect" : [ 33.0, 290.0, 15.0, 126.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 100.0, 0.0, 10.0, 120.0 ],
					"viewvisibility" : 1
				}

			}
 ],
		"lines" : [ 			{
				"patchline" : 				{
					"destination" : [ "obj-41", 0 ],
					"source" : [ "obj-1", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-37", 0 ],
					"source" : [ "obj-11", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-39", 0 ],
					"midpoints" : [ 122.5, 96.0, 238.0, 96.0, 238.0, 22.0, 359.5, 22.0 ],
					"source" : [ "obj-13", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
					"destination" : [ "obj-90", 2 ],
					"midpoints" : [ 42.0, 455.0, 408.5, 455.0 ],
					"source" : [ "obj-2", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-2", 0 ],
					"midpoints" : [ 417.5, 245.0, 42.0, 245.0 ],
					"order" : 1,
					"source" : [ "obj-21", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-3", 0 ],
					"midpoints" : [ 428.0, 245.0, 87.0, 245.0 ],
					"order" : 1,
					"source" : [ "obj-21", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-4", 0 ],
					"midpoints" : [ 438.5, 245.0, 132.0, 245.0 ],
					"order" : 1,
					"source" : [ "obj-21", 2 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-5", 0 ],
					"midpoints" : [ 449.0, 245.0, 177.0, 245.0 ],
					"order" : 1,
					"source" : [ "obj-21", 3 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-6", 0 ],
					"midpoints" : [ 459.5, 245.0, 222.0, 245.0 ],
					"order" : 1,
					"source" : [ "obj-21", 4 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-7", 0 ],
					"midpoints" : [ 470.0, 245.0, 267.0, 245.0 ],
					"order" : 1,
					"source" : [ "obj-21", 5 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-8", 0 ],
					"midpoints" : [ 480.5, 245.0, 312.0, 245.0 ],
					"order" : 1,
					"source" : [ "obj-21", 6 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
					"destination" : [ "obj-90", 13 ],
					"midpoints" : [ 480.5, 396.0, 524.0, 396.0 ],
					"order" : 0,
					"source" : [ "obj-21", 6 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
					"destination" : [ "obj-90", 11 ],
					"midpoints" : [ 470.0, 397.0, 503.0, 397.0 ],
					"order" : 0,
					"source" : [ "obj-21", 5 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
					"destination" : [ "obj-90", 9 ],
					"midpoints" : [ 459.5, 396.0, 482.0, 396.0 ],
					"order" : 0,
					"source" : [ "obj-21", 4 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
					"destination" : [ "obj-90", 7 ],
					"midpoints" : [ 449.0, 396.0, 461.0, 396.0 ],
					"order" : 0,
					"source" : [ "obj-21", 3 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
					"destination" : [ "obj-90", 5 ],
					"midpoints" : [ 438.5, 239.0, 440.0, 239.0 ],
					"order" : 0,
					"source" : [ "obj-21", 2 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
					"destination" : [ "obj-90", 3 ],
					"midpoints" : [ 428.0, 395.0, 419.0, 395.0 ],
					"order" : 0,
					"source" : [ "obj-21", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.309803921568627, 0.490196078431373, 0.611764705882353, 1.0 ],
					"destination" : [ "obj-90", 1 ],
					"midpoints" : [ 417.5, 396.0, 398.0, 396.0 ],
					"order" : 0,
					"source" : [ "obj-21", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
					"destination" : [ "obj-90", 4 ],
					"midpoints" : [ 87.0, 455.0, 429.5, 455.0 ],
					"source" : [ "obj-3", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-21", 0 ],
					"source" : [ "obj-37", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-21", 1 ],
					"midpoints" : [ 387.5, 187.0, 480.5, 187.0 ],
					"source" : [ "obj-37", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-70", 0 ],
					"source" : [ "obj-39", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
					"destination" : [ "obj-90", 6 ],
					"midpoints" : [ 132.0, 455.0, 450.5, 455.0 ],
					"source" : [ "obj-4", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-60", 0 ],
					"source" : [ "obj-41", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
					"destination" : [ "obj-90", 8 ],
					"midpoints" : [ 177.0, 455.0, 471.5, 455.0 ],
					"source" : [ "obj-5", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-91", 0 ],
					"source" : [ "obj-57", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
					"destination" : [ "obj-90", 10 ],
					"midpoints" : [ 222.0, 455.0, 492.5, 455.0 ],
					"source" : [ "obj-6", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-57", 1 ],
					"midpoints" : [ 593.0, 516.0, 662.5, 516.0 ],
					"source" : [ "obj-60", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-57", 0 ],
					"midpoints" : [ 582.5, 517.0, 507.5, 517.0 ],
					"source" : [ "obj-60", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
					"destination" : [ "obj-90", 12 ],
					"midpoints" : [ 267.0, 455.0, 513.5, 455.0 ],
					"source" : [ "obj-7", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-11", 0 ],
					"source" : [ "obj-70", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-90", 0 ],
					"midpoints" : [ 359.5, 215.0, 387.5, 215.0 ],
					"source" : [ "obj-70", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"color" : [ 0.666666666666667, 0.2, 0.2, 1.0 ],
					"destination" : [ "obj-90", 14 ],
					"midpoints" : [ 312.0, 455.0, 534.5, 455.0 ],
					"source" : [ "obj-8", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-13", 0 ],
					"source" : [ "obj-9", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-57", 0 ],
					"source" : [ "obj-90", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-90", 0 ],
					"midpoints" : [ 297.5, 214.0, 387.5, 214.0 ],
					"source" : [ "obj-92", 0 ]
				}

			}
 ],
		"parameters" : 		{
			"obj-2::obj-41::obj-2" : [ "live.toggle", "live.toggle", 0 ],
			"obj-2::obj-50::obj-2" : [ "live.toggle[1]", "live.toggle", 0 ],
			"obj-2::obj-51::obj-2" : [ "live.toggle[2]", "live.toggle", 0 ],
			"obj-2::obj-52::obj-2" : [ "live.toggle[3]", "live.toggle", 0 ],
			"obj-2::obj-53::obj-2" : [ "live.toggle[4]", "live.toggle", 0 ],
			"obj-2::obj-54::obj-2" : [ "live.toggle[5]", "live.toggle", 0 ],
			"obj-2::obj-55::obj-2" : [ "live.toggle[6]", "live.toggle", 0 ],
			"obj-2::obj-56::obj-2" : [ "live.toggle[7]", "live.toggle", 0 ],
			"obj-2::obj-57::obj-2" : [ "live.toggle[8]", "live.toggle", 0 ],
			"obj-2::obj-58::obj-2" : [ "live.toggle[9]", "live.toggle", 0 ],
			"obj-2::obj-59::obj-2" : [ "live.toggle[10]", "live.toggle", 0 ],
			"obj-2::obj-60::obj-2" : [ "live.toggle[11]", "live.toggle", 0 ],
			"obj-3::obj-41::obj-2" : [ "live.toggle[12]", "live.toggle", 0 ],
			"obj-3::obj-50::obj-2" : [ "live.toggle[13]", "live.toggle", 0 ],
			"obj-3::obj-51::obj-2" : [ "live.toggle[14]", "live.toggle", 0 ],
			"obj-3::obj-52::obj-2" : [ "live.toggle[15]", "live.toggle", 0 ],
			"obj-3::obj-53::obj-2" : [ "live.toggle[16]", "live.toggle", 0 ],
			"obj-3::obj-54::obj-2" : [ "live.toggle[17]", "live.toggle", 0 ],
			"obj-3::obj-55::obj-2" : [ "live.toggle[18]", "live.toggle", 0 ],
			"obj-3::obj-56::obj-2" : [ "live.toggle[19]", "live.toggle", 0 ],
			"obj-3::obj-57::obj-2" : [ "live.toggle[20]", "live.toggle", 0 ],
			"obj-3::obj-58::obj-2" : [ "live.toggle[21]", "live.toggle", 0 ],
			"obj-3::obj-59::obj-2" : [ "live.toggle[22]", "live.toggle", 0 ],
			"obj-3::obj-60::obj-2" : [ "live.toggle[23]", "live.toggle", 0 ],
			"obj-41" : [ "live.dial", "Tonic", 0 ],
			"obj-4::obj-41::obj-2" : [ "live.toggle[24]", "live.toggle", 0 ],
			"obj-4::obj-50::obj-2" : [ "live.toggle[25]", "live.toggle", 0 ],
			"obj-4::obj-51::obj-2" : [ "live.toggle[26]", "live.toggle", 0 ],
			"obj-4::obj-52::obj-2" : [ "live.toggle[27]", "live.toggle", 0 ],
			"obj-4::obj-53::obj-2" : [ "live.toggle[28]", "live.toggle", 0 ],
			"obj-4::obj-54::obj-2" : [ "live.toggle[29]", "live.toggle", 0 ],
			"obj-4::obj-55::obj-2" : [ "live.toggle[30]", "live.toggle", 0 ],
			"obj-4::obj-56::obj-2" : [ "live.toggle[31]", "live.toggle", 0 ],
			"obj-4::obj-57::obj-2" : [ "live.toggle[32]", "live.toggle", 0 ],
			"obj-4::obj-58::obj-2" : [ "live.toggle[33]", "live.toggle", 0 ],
			"obj-4::obj-59::obj-2" : [ "live.toggle[34]", "live.toggle", 0 ],
			"obj-4::obj-60::obj-2" : [ "live.toggle[35]", "live.toggle", 0 ],
			"obj-5::obj-41::obj-2" : [ "live.toggle[36]", "live.toggle", 0 ],
			"obj-5::obj-50::obj-2" : [ "live.toggle[37]", "live.toggle", 0 ],
			"obj-5::obj-51::obj-2" : [ "live.toggle[38]", "live.toggle", 0 ],
			"obj-5::obj-52::obj-2" : [ "live.toggle[39]", "live.toggle", 0 ],
			"obj-5::obj-53::obj-2" : [ "live.toggle[40]", "live.toggle", 0 ],
			"obj-5::obj-54::obj-2" : [ "live.toggle[41]", "live.toggle", 0 ],
			"obj-5::obj-55::obj-2" : [ "live.toggle[42]", "live.toggle", 0 ],
			"obj-5::obj-56::obj-2" : [ "live.toggle[43]", "live.toggle", 0 ],
			"obj-5::obj-57::obj-2" : [ "live.toggle[44]", "live.toggle", 0 ],
			"obj-5::obj-58::obj-2" : [ "live.toggle[45]", "live.toggle", 0 ],
			"obj-5::obj-59::obj-2" : [ "live.toggle[46]", "live.toggle", 0 ],
			"obj-5::obj-60::obj-2" : [ "live.toggle[47]", "live.toggle", 0 ],
			"obj-6::obj-41::obj-2" : [ "live.toggle[48]", "live.toggle", 0 ],
			"obj-6::obj-50::obj-2" : [ "live.toggle[49]", "live.toggle", 0 ],
			"obj-6::obj-51::obj-2" : [ "live.toggle[50]", "live.toggle", 0 ],
			"obj-6::obj-52::obj-2" : [ "live.toggle[51]", "live.toggle", 0 ],
			"obj-6::obj-53::obj-2" : [ "live.toggle[52]", "live.toggle", 0 ],
			"obj-6::obj-54::obj-2" : [ "live.toggle[53]", "live.toggle", 0 ],
			"obj-6::obj-55::obj-2" : [ "live.toggle[54]", "live.toggle", 0 ],
			"obj-6::obj-56::obj-2" : [ "live.toggle[55]", "live.toggle", 0 ],
			"obj-6::obj-57::obj-2" : [ "live.toggle[56]", "live.toggle", 0 ],
			"obj-6::obj-58::obj-2" : [ "live.toggle[57]", "live.toggle", 0 ],
			"obj-6::obj-59::obj-2" : [ "live.toggle[58]", "live.toggle", 0 ],
			"obj-6::obj-60::obj-2" : [ "live.toggle[59]", "live.toggle", 0 ],
			"obj-7::obj-41::obj-2" : [ "live.toggle[60]", "live.toggle", 0 ],
			"obj-7::obj-50::obj-2" : [ "live.toggle[61]", "live.toggle", 0 ],
			"obj-7::obj-51::obj-2" : [ "live.toggle[62]", "live.toggle", 0 ],
			"obj-7::obj-52::obj-2" : [ "live.toggle[63]", "live.toggle", 0 ],
			"obj-7::obj-53::obj-2" : [ "live.toggle[64]", "live.toggle", 0 ],
			"obj-7::obj-54::obj-2" : [ "live.toggle[65]", "live.toggle", 0 ],
			"obj-7::obj-55::obj-2" : [ "live.toggle[66]", "live.toggle", 0 ],
			"obj-7::obj-56::obj-2" : [ "live.toggle[67]", "live.toggle", 0 ],
			"obj-7::obj-57::obj-2" : [ "live.toggle[68]", "live.toggle", 0 ],
			"obj-7::obj-58::obj-2" : [ "live.toggle[69]", "live.toggle", 0 ],
			"obj-7::obj-59::obj-2" : [ "live.toggle[70]", "live.toggle", 0 ],
			"obj-7::obj-60::obj-2" : [ "live.toggle[71]", "live.toggle", 0 ],
			"obj-8::obj-41::obj-2" : [ "live.toggle[72]", "live.toggle", 0 ],
			"obj-8::obj-50::obj-2" : [ "live.toggle[73]", "live.toggle", 0 ],
			"obj-8::obj-51::obj-2" : [ "live.toggle[74]", "live.toggle", 0 ],
			"obj-8::obj-52::obj-2" : [ "live.toggle[75]", "live.toggle", 0 ],
			"obj-8::obj-53::obj-2" : [ "live.toggle[76]", "live.toggle", 0 ],
			"obj-8::obj-54::obj-2" : [ "live.toggle[77]", "live.toggle", 0 ],
			"obj-8::obj-55::obj-2" : [ "live.toggle[78]", "live.toggle", 0 ],
			"obj-8::obj-56::obj-2" : [ "live.toggle[79]", "live.toggle", 0 ],
			"obj-8::obj-57::obj-2" : [ "live.toggle[80]", "live.toggle", 0 ],
			"obj-8::obj-58::obj-2" : [ "live.toggle[81]", "live.toggle", 0 ],
			"obj-8::obj-59::obj-2" : [ "live.toggle[82]", "live.toggle", 0 ],
			"obj-8::obj-60::obj-2" : [ "live.toggle[83]", "live.toggle", 0 ],
			"parameterbanks" : 			{

			}
,
			"parameter_overrides" : 			{
				"obj-2::obj-50::obj-2" : 				{
					"parameter_longname" : "live.toggle[1]"
				}
,
				"obj-2::obj-51::obj-2" : 				{
					"parameter_longname" : "live.toggle[2]"
				}
,
				"obj-2::obj-52::obj-2" : 				{
					"parameter_longname" : "live.toggle[3]"
				}
,
				"obj-2::obj-53::obj-2" : 				{
					"parameter_longname" : "live.toggle[4]"
				}
,
				"obj-2::obj-54::obj-2" : 				{
					"parameter_longname" : "live.toggle[5]"
				}
,
				"obj-2::obj-55::obj-2" : 				{
					"parameter_longname" : "live.toggle[6]"
				}
,
				"obj-2::obj-56::obj-2" : 				{
					"parameter_longname" : "live.toggle[7]"
				}
,
				"obj-2::obj-57::obj-2" : 				{
					"parameter_longname" : "live.toggle[8]"
				}
,
				"obj-2::obj-58::obj-2" : 				{
					"parameter_longname" : "live.toggle[9]"
				}
,
				"obj-2::obj-59::obj-2" : 				{
					"parameter_longname" : "live.toggle[10]"
				}
,
				"obj-2::obj-60::obj-2" : 				{
					"parameter_longname" : "live.toggle[11]"
				}
,
				"obj-3::obj-41::obj-2" : 				{
					"parameter_longname" : "live.toggle[12]"
				}
,
				"obj-3::obj-50::obj-2" : 				{
					"parameter_longname" : "live.toggle[13]"
				}
,
				"obj-3::obj-51::obj-2" : 				{
					"parameter_longname" : "live.toggle[14]"
				}
,
				"obj-3::obj-52::obj-2" : 				{
					"parameter_longname" : "live.toggle[15]"
				}
,
				"obj-3::obj-53::obj-2" : 				{
					"parameter_longname" : "live.toggle[16]"
				}
,
				"obj-3::obj-54::obj-2" : 				{
					"parameter_longname" : "live.toggle[17]"
				}
,
				"obj-3::obj-55::obj-2" : 				{
					"parameter_longname" : "live.toggle[18]"
				}
,
				"obj-3::obj-56::obj-2" : 				{
					"parameter_longname" : "live.toggle[19]"
				}
,
				"obj-3::obj-57::obj-2" : 				{
					"parameter_longname" : "live.toggle[20]"
				}
,
				"obj-3::obj-58::obj-2" : 				{
					"parameter_longname" : "live.toggle[21]"
				}
,
				"obj-3::obj-59::obj-2" : 				{
					"parameter_longname" : "live.toggle[22]"
				}
,
				"obj-3::obj-60::obj-2" : 				{
					"parameter_longname" : "live.toggle[23]"
				}
,
				"obj-4::obj-41::obj-2" : 				{
					"parameter_longname" : "live.toggle[24]"
				}
,
				"obj-4::obj-50::obj-2" : 				{
					"parameter_longname" : "live.toggle[25]"
				}
,
				"obj-4::obj-51::obj-2" : 				{
					"parameter_longname" : "live.toggle[26]"
				}
,
				"obj-4::obj-52::obj-2" : 				{
					"parameter_longname" : "live.toggle[27]"
				}
,
				"obj-4::obj-53::obj-2" : 				{
					"parameter_longname" : "live.toggle[28]"
				}
,
				"obj-4::obj-54::obj-2" : 				{
					"parameter_longname" : "live.toggle[29]"
				}
,
				"obj-4::obj-55::obj-2" : 				{
					"parameter_longname" : "live.toggle[30]"
				}
,
				"obj-4::obj-56::obj-2" : 				{
					"parameter_longname" : "live.toggle[31]"
				}
,
				"obj-4::obj-57::obj-2" : 				{
					"parameter_longname" : "live.toggle[32]"
				}
,
				"obj-4::obj-58::obj-2" : 				{
					"parameter_longname" : "live.toggle[33]"
				}
,
				"obj-4::obj-59::obj-2" : 				{
					"parameter_longname" : "live.toggle[34]"
				}
,
				"obj-4::obj-60::obj-2" : 				{
					"parameter_longname" : "live.toggle[35]"
				}
,
				"obj-5::obj-41::obj-2" : 				{
					"parameter_longname" : "live.toggle[36]"
				}
,
				"obj-5::obj-50::obj-2" : 				{
					"parameter_longname" : "live.toggle[37]"
				}
,
				"obj-5::obj-51::obj-2" : 				{
					"parameter_longname" : "live.toggle[38]"
				}
,
				"obj-5::obj-52::obj-2" : 				{
					"parameter_longname" : "live.toggle[39]"
				}
,
				"obj-5::obj-53::obj-2" : 				{
					"parameter_longname" : "live.toggle[40]"
				}
,
				"obj-5::obj-54::obj-2" : 				{
					"parameter_longname" : "live.toggle[41]"
				}
,
				"obj-5::obj-55::obj-2" : 				{
					"parameter_longname" : "live.toggle[42]"
				}
,
				"obj-5::obj-56::obj-2" : 				{
					"parameter_longname" : "live.toggle[43]"
				}
,
				"obj-5::obj-57::obj-2" : 				{
					"parameter_longname" : "live.toggle[44]"
				}
,
				"obj-5::obj-58::obj-2" : 				{
					"parameter_longname" : "live.toggle[45]"
				}
,
				"obj-5::obj-59::obj-2" : 				{
					"parameter_longname" : "live.toggle[46]"
				}
,
				"obj-5::obj-60::obj-2" : 				{
					"parameter_longname" : "live.toggle[47]"
				}
,
				"obj-6::obj-41::obj-2" : 				{
					"parameter_longname" : "live.toggle[48]"
				}
,
				"obj-6::obj-50::obj-2" : 				{
					"parameter_longname" : "live.toggle[49]"
				}
,
				"obj-6::obj-51::obj-2" : 				{
					"parameter_longname" : "live.toggle[50]"
				}
,
				"obj-6::obj-52::obj-2" : 				{
					"parameter_longname" : "live.toggle[51]"
				}
,
				"obj-6::obj-53::obj-2" : 				{
					"parameter_longname" : "live.toggle[52]"
				}
,
				"obj-6::obj-54::obj-2" : 				{
					"parameter_longname" : "live.toggle[53]"
				}
,
				"obj-6::obj-55::obj-2" : 				{
					"parameter_longname" : "live.toggle[54]"
				}
,
				"obj-6::obj-56::obj-2" : 				{
					"parameter_longname" : "live.toggle[55]"
				}
,
				"obj-6::obj-57::obj-2" : 				{
					"parameter_longname" : "live.toggle[56]"
				}
,
				"obj-6::obj-58::obj-2" : 				{
					"parameter_longname" : "live.toggle[57]"
				}
,
				"obj-6::obj-59::obj-2" : 				{
					"parameter_longname" : "live.toggle[58]"
				}
,
				"obj-6::obj-60::obj-2" : 				{
					"parameter_longname" : "live.toggle[59]"
				}
,
				"obj-7::obj-41::obj-2" : 				{
					"parameter_longname" : "live.toggle[60]"
				}
,
				"obj-7::obj-50::obj-2" : 				{
					"parameter_longname" : "live.toggle[61]"
				}
,
				"obj-7::obj-51::obj-2" : 				{
					"parameter_longname" : "live.toggle[62]"
				}
,
				"obj-7::obj-52::obj-2" : 				{
					"parameter_longname" : "live.toggle[63]"
				}
,
				"obj-7::obj-53::obj-2" : 				{
					"parameter_longname" : "live.toggle[64]"
				}
,
				"obj-7::obj-54::obj-2" : 				{
					"parameter_longname" : "live.toggle[65]"
				}
,
				"obj-7::obj-55::obj-2" : 				{
					"parameter_longname" : "live.toggle[66]"
				}
,
				"obj-7::obj-56::obj-2" : 				{
					"parameter_longname" : "live.toggle[67]"
				}
,
				"obj-7::obj-57::obj-2" : 				{
					"parameter_longname" : "live.toggle[68]"
				}
,
				"obj-7::obj-58::obj-2" : 				{
					"parameter_longname" : "live.toggle[69]"
				}
,
				"obj-7::obj-59::obj-2" : 				{
					"parameter_longname" : "live.toggle[70]"
				}
,
				"obj-7::obj-60::obj-2" : 				{
					"parameter_longname" : "live.toggle[71]"
				}
,
				"obj-8::obj-41::obj-2" : 				{
					"parameter_longname" : "live.toggle[72]"
				}
,
				"obj-8::obj-50::obj-2" : 				{
					"parameter_longname" : "live.toggle[73]"
				}
,
				"obj-8::obj-51::obj-2" : 				{
					"parameter_longname" : "live.toggle[74]"
				}
,
				"obj-8::obj-52::obj-2" : 				{
					"parameter_longname" : "live.toggle[75]"
				}
,
				"obj-8::obj-53::obj-2" : 				{
					"parameter_longname" : "live.toggle[76]"
				}
,
				"obj-8::obj-54::obj-2" : 				{
					"parameter_longname" : "live.toggle[77]"
				}
,
				"obj-8::obj-55::obj-2" : 				{
					"parameter_longname" : "live.toggle[78]"
				}
,
				"obj-8::obj-56::obj-2" : 				{
					"parameter_longname" : "live.toggle[79]"
				}
,
				"obj-8::obj-57::obj-2" : 				{
					"parameter_longname" : "live.toggle[80]"
				}
,
				"obj-8::obj-58::obj-2" : 				{
					"parameter_longname" : "live.toggle[81]"
				}
,
				"obj-8::obj-59::obj-2" : 				{
					"parameter_longname" : "live.toggle[82]"
				}
,
				"obj-8::obj-60::obj-2" : 				{
					"parameter_longname" : "live.toggle[83]"
				}

			}
,
			"inherited_shortname" : 1
		}
,
		"dependency_cache" : [ 			{
				"name" : "edv.noteselector.maxpat",
				"bootpath" : "~/Music/Ableton/User Library/Presets/MIDI Effects/Max MIDI Effect",
				"patcherrelativepath" : ".",
				"type" : "JSON",
				"implicit" : 1
			}
, 			{
				"name" : "edv.radiobutton.maxpat",
				"bootpath" : "~/Music/Ableton/User Library/Presets/MIDI Effects/Max MIDI Effect",
				"patcherrelativepath" : ".",
				"type" : "JSON",
				"implicit" : 1
			}
, 			{
				"name" : "edv.scaleselector-scales.txt",
				"bootpath" : "~/Music/Ableton/User Library/Presets/MIDI Effects/Max MIDI Effect",
				"patcherrelativepath" : ".",
				"type" : "TEXT",
				"implicit" : 1
			}
 ],
		"autosave" : 0
	}

}
