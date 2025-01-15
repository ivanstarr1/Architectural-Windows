ECSExplorerDialog : dialog
{
  label = "ECS Explorer";
  	: boxed_row {
  		label = "ECS-RElated Data";
  			: list_box {
  				key = "lstECSData";
				  tabs = "26";			
				  height = 4;				
				  width = 120;
  			}
  	}
  	: boxed_row {
  		label = "ECS Transform";
 			: list_box {
  				key = "lstECSXForm";
				  tabs = "2 11 11 11 11";			
				  height = 5;				
				  width = 120;
  			}
  	}
  	: boxed_row {
 			label = "All DXF Data";
  			: list_box {
  				key = "lstDXF";
				  tabs = "26";			//set tabs
				  height = 30;				//give it a height
				  width = 120;
  			}
  	}

    ok_only ;				//predefined OK/Cancel button
}