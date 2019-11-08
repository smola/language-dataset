{
	"Name": "12+ Expressions",
	"Expressions": [{
			"Name": "Cycle All",
			"RegStandard": 0,
			"ExpressionEnum": 1024,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 50,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": 5440,
					"Outputs": []
				}
			],
			"Entries": [{
					"ExpressionEnum": 1027,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1028,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1029,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1030,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1031,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1032,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1033,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1034,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1035,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1036,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1037,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1038,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1039,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1040,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1040,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1041,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1042,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1043,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1044,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1045,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1046,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1047,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1054,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1055,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1056,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1057,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1058,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1059,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1060,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1061,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1070,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1071,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1072,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1073,
					"Repeats": 2
				}, {
					"ExpressionEnum": 1106,
					"Repeats": 2
				}
			]
		}, {
			"Name": "Lightbar_Steady_PWM_50",
			"RegStandard": 0,
			"ExpressionEnum": 1026,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 50,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Double_75_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1027,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 380,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 2
				}, {
					"Period": 160,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}, {
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Title_13_Quad_65_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1028,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 303,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 115,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Title_13_Double_65_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1029,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 363,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 2
				}, {
					"Period": 230,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}, {
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Quint_Hold_75_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1030,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 400,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {}, {
					"Period": 200,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Pulse_8_Burst_75_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1031,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 472,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 8
				}, {
					"Period": 25,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}, {
					"Period": 16,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Reg_65_Single_120_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1032,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 300,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Period": 200,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Reg_65_Double_120_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1033,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 270,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 2
				}, {
					"Period": 80,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}, {
					"Period": 35,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Reg_65_Triple_120_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1034,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 284,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}, {
					"Period": 22,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Reg_65_Quad_120_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1035,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 288,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 35,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}, {
					"Period": 18,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Reg_65_Burst_120_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1036,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 292,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 8
				}, {
					"Period": 13,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}, {
					"Period": 13,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Reg_65_Single_S_S_120_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1037,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Period": 200,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_100
						}
					]
				}, {
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_0
						}
					]
				}, {
					"Period": 200,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Reg_65_Single_S_S_120_FPM_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1054,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Period": 195,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_100
						}
					]
				}, {
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_0
						}
					]
				}, {
					"Period": 195,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Reg_65_Double_S_S_120_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1038,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 2
				}, {
					"Period": 80,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_100
						}
					]
				}, {
					"Period": 35,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_0
						}
					]
				}, {}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": NONE_0
						}
					]
				}, {
					"Repeats": 2
				}, {
					"Period": 80,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_100
						}
					]
				}, {
					"Period": 35,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Reg_65_Double_S_S_120_FPM_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1055,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 2
				}, {
					"Period": 80,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_100
						}
					]
				}, {
					"Period": 35,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_0
						}
					]
				}, {}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": NONE_0
						}
					]
				}, {
					"Repeats": 2
				}, {
					"Period": 80,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_100
						}
					]
				}, {
					"Period": 35,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Reg_65_Triple_S_S_120_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1039,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 34,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_100
						}
					]
				}, {
					"Period": 22,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_0
						}
					]
				}, {}, {
					"Period": 34,
					"Tokens": [{
							"Key": 5007,
							"Value": NONE_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_100
						}
					]
				}, {
					"Period": 22,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Reg_65_Triple_S_S_120_FPM_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1056,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 34,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_100
						}
					]
				}, {
					"Period": 22,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_0
						}
					]
				}, {}, {
					"Period": 34,
					"Tokens": [{
							"Key": 5007,
							"Value": NONE_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_100
						}
					]
				}, {
					"Period": 22,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Reg_65_Quad_S_S_120_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1040,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 38,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 35,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_100
						}
					]
				}, {
					"Period": 18,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_0
						}
					]
				}, {}, {
					"Period": 38,
					"Tokens": [{
							"Key": 5007,
							"Value": NONE_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 35,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_100
						}
					]
				}, {
					"Period": 18,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Reg_65_Quad_S_S_120_FPM_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1057,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 38,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 35,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_100
						}
					]
				}, {
					"Period": 18,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_0
						}
					]
				}, {}, {
					"Period": 38,
					"Tokens": [{
							"Key": 5007,
							"Value": NONE_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 35,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_100
						}
					]
				}, {
					"Period": 18,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Reg_65_Burst_S_S_120_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1041,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 42,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 8
				}, {
					"Period": 13,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_100
						}
					]
				}, {
					"Period": 13,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_0
						}
					]
				}, {}, {
					"Period": 42,
					"Tokens": [{
							"Key": 5007,
							"Value": NONE_0
						}
					]
				}, {
					"Repeats": 8
				}, {
					"Period": 13,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_100
						}
					]
				}, {
					"Period": 13,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Reg_65_Burst_S_S_120_FPM_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1058,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 42,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 8
				}, {
					"Period": 13,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_100
						}
					]
				}, {
					"Period": 13,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_0
						}
					]
				}, {}, {
					"Period": 42,
					"Tokens": [{
							"Key": 5007,
							"Value": NONE_0
						}
					]
				}, {
					"Repeats": 8
				}, {
					"Period": 13,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_100
						}
					]
				}, {
					"Period": 13,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Quad_Alternate_S_S_150_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1042,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Quad_Alternate_S_S_150_FPM_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1059,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Quad_Cross_Alternate_150_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1043,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_A_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_A_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_A_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_A_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_B_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_B_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_B_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Double_Alternate_S_S_150_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1044,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Double_Alternate_S_S_150_FPM_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1060,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Double_Cross_Alternate_150_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1045,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_A_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_A_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_A_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_A_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_B_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_B_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_B_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Quint_Hold_Alternate_S_S_150_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1046,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Quint_Hold_Alternate_S_S_150_FPM_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1061,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": LEFT_CENTER_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": RIGHT_CENTER_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Quint_Hold_Cross_Alternate_150_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1047,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_A_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_A_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_A_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_A_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_B_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_B_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": CROSS_B_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Fast_Rotate",
			"RegStandard": 0,
			"ExpressionEnum": 1070,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Period": 799,
					"Tokens": [{
							"Key": 5007,
							"Value": FAST_ROTATE_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Wave_Rotate",
			"RegStandard": 0,
			"ExpressionEnum": 1071,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Period": 799,
					"Tokens": [{
							"Key": 5007,
							"Value": WAVE_ROTATE_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Rotate_Quad",
			"RegStandard": 0,
			"ExpressionEnum": 1072,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Period": 119,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_0
						}
					]
				}, {
					"Period": 119,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_0
						}
					]
				}, {
					"Period": 119,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_0
						}
					]
				}, {
					"Period": 119,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Fast_Quad",
			"RegStandard": 0,
			"ExpressionEnum": 1073,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 120,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Airport_Flash_75_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1106,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 50,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FULL_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 400,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_0
						}
					]
				}, {
					"Period": 400,
					"Tokens": [{
							"Key": 5007,
							"Value": FULL_BAR_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Rear_Center_Double_75_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1069,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": REAR_CENTER_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 380,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_CENTER_0
						}
					]
				}, {
					"Repeats": 2
				}, {
					"Period": 160,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_CENTER_100
						}
					]
				}, {
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_CENTER_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Rear_Quad_Alternate_S_S_150_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1049,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": REAR_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_BAR_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Rear_Quad_Alternate_S_S_150_FPM_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1063,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": REAR_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_BAR_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_CENTER_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_CENTER_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_CENTER_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_CENTER_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_CENTER_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_CENTER_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_CENTER_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Rear_Double_Alternate_S_S_150_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1051,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": REAR_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_BAR_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Rear_Double_Alternate_S_S_150_FPM_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1065,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": REAR_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_BAR_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_CENTER_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_CENTER_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_CENTER_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_CENTER_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_CENTER_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_CENTER_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_CENTER_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Rear_Quint_Hold_Alternate_Side_to_Side",
			"RegStandard": 0,
			"ExpressionEnum": 1053,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": REAR_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_BAR_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Rear_Quint_Hold_Alternate_Side_to_Side_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1067,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": REAR_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_BAR_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_CENTER_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_CENTER_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_CENTER_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_LEFT_CENTER_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_CENTER_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_CENTER_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": REAR_RIGHT_CENTER_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Front_Center_Double_75_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1068,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FRONT_CENTER_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 380,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_CENTER_0
						}
					]
				}, {
					"Repeats": 2
				}, {
					"Period": 160,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_CENTER_100
						}
					]
				}, {
					"Period": 50,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_CENTER_0
						}
					]
				}, {}
			]
		}, {
			"Name": "Lightbar_Front_Quad_Alternate_S_S_150_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1048,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FRONT_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_BAR_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Front_Quad_Alternate_S_S_150_FPM_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1062,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FRONT_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_BAR_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_CENTER_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_CENTER_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_CENTER_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_CENTER_0
						}
					]
				}, {
					"Repeats": 3
				}, {
					"Period": 70,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_CENTER_100
						}
					]
				}, {
					"Period": 40,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_CENTER_0
						}
					]
				}, {}, {
					"Period": 69,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_CENTER_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Front_Double_Alternate_S_S_150_FPM",
			"RegStandard": 0,
			"ExpressionEnum": 1050,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FRONT_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_BAR_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Front_Double_Alternate_S_S_150_FPM_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1064,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FRONT_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_BAR_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_CENTER_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_CENTER_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_CENTER_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_CENTER_0
						}
					]
				}, {
					"Period": 170,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_CENTER_100
						}
					]
				}, {
					"Period": 60,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_CENTER_0
						}
					]
				}, {
					"Period": 169,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_CENTER_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Front_Quint_Hold_Alternate_Side_to_Side",
			"RegStandard": 0,
			"ExpressionEnum": 1052,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FRONT_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_BAR_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_100
						}
					]
				}
			]
		}, {
			"Name": "Lightbar_Front_Quint_Hold_Alternate_Side_to_Side_Center_Pulse",
			"RegStandard": 0,
			"ExpressionEnum": 1066,
			"Repeats": 0,
			"InputPriority": 1,
			"OutputPriority": 1,
			"Sequencer": 1,
			"Value": 100,
			"Areas": [{
					"Name": "Dictionary",
					"Key": 5007,
					"DefaultValue": FRONT_BAR_0,
					"Outputs": []
				}
			],
			"Entries": [{
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_BAR_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_CENTER_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_CENTER_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_CENTER_100
						}
					]
				}, {
					"Period": 1,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_LEFT_CENTER_0
						}
					]
				}, {
					"Repeats": 4
				}, {
					"Period": 30,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_CENTER_100
						}
					]
				}, {
					"Period": 20,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_CENTER_0
						}
					]
				}, {}, {
					"Period": 199,
					"Tokens": [{
							"Key": 5007,
							"Value": FRONT_RIGHT_CENTER_100
						}
					]
				}
			]
		}
	]
}
