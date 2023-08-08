within ;
package GlucoseInsulin
  package CompartmentApproach
    package Components
      model GlucoseCompartment
        Modelica.Blocks.Interfaces.RealInput glucoseInflow "mg/h" annotation (Placement(
              transformation(extent={{-408,-30},{-368,10}}), iconTransformation(
                extent={{-126,-12},{-100,14}})));
        Modelica.Blocks.Interfaces.RealInput insulinIndependentOutflow "mg/h" annotation (
            Placement(transformation(extent={{-400,-36},{-380,-16}}),
              iconTransformation(extent={{-10,-10},{10,10}},
              rotation=180,
              origin={110,0})));
        Modelica.Blocks.Interfaces.RealInput renalOutflow "mg/h" annotation (Placement(
              transformation(extent={{-400,-36},{-380,-16}}), iconTransformation(
                extent={{-10,-10},{10,10}},
              rotation=180,
              origin={110,60})));
        Modelica.Blocks.Interfaces.RealInput insulinDependentOutflow "mg/h" annotation (
            Placement(transformation(extent={{-400,-36},{-380,-16}}),
              iconTransformation(extent={{-10,-10},{10,10}},
              rotation=180,
              origin={110,-66})));
        Modelica.Blocks.Interfaces.RealInput volume "ml" annotation (Placement(
              transformation(extent={{-402,-24},{-362,16}}), iconTransformation(
              extent={{-20,-20},{20,20}},
              rotation=270,
              origin={-2,102})));
        Modelica.Blocks.Interfaces.RealOutput glucoseConcentration "mg/h" annotation (
            Placement(transformation(extent={{-562,-74},{-542,-54}}),
              iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={48,-110})));
        Modelica.Blocks.Interfaces.RealOutput glucoseContent( start = initialContent) "mg" annotation (Placement(
              transformation(extent={{-562,-74},{-542,-54}}), iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-52,-110})));
        parameter Real initialContent = 12165 "mg";
      equation
        der(glucoseContent) = glucoseInflow - renalOutflow - insulinIndependentOutflow - insulinDependentOutflow;
        glucoseConcentration = glucoseContent/volume;
        annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
                Rectangle(
                extent={{-100,100},{100,-100}},
                lineColor={28,108,200},
                fillColor={28,108,200},
                fillPattern=FillPattern.None,
                lineThickness=1), Rectangle(
                extent={{-100,48},{100,-100}},
                lineColor={28,108,200},
                lineThickness=1,
                fillColor={28,108,200},
                fillPattern=FillPattern.Solid),
              Text(
                extent={{-122,-112},{90,-156}},
                textColor={28,108,200},
                textString="%name")}),            Diagram(coordinateSystem(
                preserveAspectRatio=false)));
      end GlucoseCompartment;

      model InsulinCompartment
        Modelica.Blocks.Interfaces.RealInput insulinInflow "mU/h" annotation (Placement(
              transformation(extent={{-408,-30},{-368,10}}), iconTransformation(
                extent={{-134,-18},{-100,16}})));
        Modelica.Blocks.Interfaces.RealInput insulinDestructionRate "mU/h" annotation (
            Placement(transformation(extent={{-400,-36},{-380,-16}}),
              iconTransformation(extent={{-13,-13},{13,13}},
              rotation=180,
              origin={113,1})));
        Modelica.Blocks.Interfaces.RealInput volume "ml" annotation (Placement(
              transformation(extent={{-402,-24},{-362,16}}), iconTransformation(
              extent={{-20,-20},{20,20}},
              rotation=270,
              origin={-2,104})));
        Modelica.Blocks.Interfaces.RealOutput insulinConcentration "mU/ml" annotation (
            Placement(transformation(extent={{-562,-74},{-542,-54}}),
              iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={48,-110})));
        Modelica.Blocks.Interfaces.RealOutput insulinContent( start = initialInsulinContent) "mU" annotation (Placement(
              transformation(extent={{-562,-74},{-542,-54}}), iconTransformation(
              extent={{-10,-10},{10,10}},
              rotation=270,
              origin={-54,-110})));
        parameter Real initialInsulinContent=851 "mU";
      equation
        der(insulinContent) =insulinInflow - insulinDestructionRate;
        insulinConcentration =insulinContent /volume;
        annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
                Rectangle(
                extent={{-100,100},{100,-100}},
                lineColor={28,108,200},
                fillColor={28,108,200},
                fillPattern=FillPattern.None,
                lineThickness=1), Rectangle(
                extent={{-100,48},{100,-100}},
                lineColor={28,108,200},
                lineThickness=1,
                fillColor={255,85,85},
                fillPattern=FillPattern.Solid),
              Text(
                extent={{-130,-128},{94,-164}},
                textColor={28,108,200},
                textString="%name")}),            Diagram(coordinateSystem(
                preserveAspectRatio=false)));
      end InsulinCompartment;

      model GlucoseInputFlowRate
        parameter Real glucoseInput=8400 "mg/h";
        Modelica.Blocks.Interfaces.RealOutput glucoseInflow "mg/h" annotation (Placement(
              transformation(extent={{-286,-20},{-266,2}}), iconTransformation(extent=
                 {{46,-16},{66,6}})));
      equation
        glucoseInput = glucoseInflow;
        annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
                Polygon(
                points={{-42,12},{-106,82},{124,80},{28,-8},{86,-90},{-8,-44},{-88,-72},
                    {-42,12}},
                lineColor={28,108,200},
                lineThickness=1,
                fillColor={170,255,255},
                fillPattern=FillPattern.Solid,
                smooth=Smooth.Bezier), Text(
                extent={{-46,68},{38,-4}},
                textColor={28,108,200},
                textString="%glucoseInput"),
              Text(
                extent={{-52,-22},{46,-40}},
                textColor={28,108,200},
                textString="mg/h"),
              Text(
                extent={{-336,-78},{322,-106}},
                textColor={28,108,200},
                textString="%name")}),   Diagram(coordinateSystem(preserveAspectRatio=
                 false)));
      end GlucoseInputFlowRate;

      model ECT_Volume
        parameter Real ECTvolume = 15000 "ml";
        Modelica.Blocks.Interfaces.RealOutput volume "ml" annotation (Placement(
              transformation(extent={{-286,-20},{-266,2}}), iconTransformation(extent=
                 {{96,-16},{116,6}})));
      equation
        volume = ECTvolume;
        annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
              Rectangle(
                extent={{-100,100},{100,-98}},
                lineColor={28,108,200},
                lineThickness=1,
                fillColor={170,213,255},
                fillPattern=FillPattern.Solid),
              Text(
                extent={{-88,-28},{70,-72}},
                textColor={28,108,200},
                textString="ml"),
              Text(
                extent={{-96,84},{84,16}},
                textColor={28,108,200},
                textString="%ECTvolume"),
              Text(
                extent={{-232,-116},{222,-148}},
                textColor={28,108,200},
                textString="%name")}),      Diagram(coordinateSystem(
                preserveAspectRatio=false)));
      end ECT_Volume;

      model RenalLoss
        Modelica.Blocks.Interfaces.RealInput glucoseConcentration "mg/ml" annotation (
           Placement(transformation(extent={{158,-28},{198,12}}), iconTransformation(
                extent={{-126,16},{-94,48}})));
        Modelica.Blocks.Interfaces.RealOutput renalLoss "mg/h" annotation (Placement(
              transformation(extent={{100,-42},{120,-22}}), iconTransformation(
              extent={{-14,-14},{14,14}},
              rotation=180,
              origin={-114,-20})));
        parameter Real glucoseConcentrationThreshold=2.5 "mg/ml";
        parameter Real GFR=7200 "ml/h";
      equation
        if glucoseConcentration < glucoseConcentrationThreshold then
          renalLoss = 0;
        else
          renalLoss = GFR*(glucoseConcentration - glucoseConcentrationThreshold);
        end if;
          annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
                Bitmap(extent={{-96,-94},{96,104}}, fileName=
                    "modelica://GlucoseInsulin/kidney.jpg"), Text(
                extent={{-190,-104},{188,-118}},
                textColor={28,108,200},
                textString="%name")}),                         Diagram(
              coordinateSystem(preserveAspectRatio=false)));
      end RenalLoss;

      model InsulinDependentGlucoseUtilisation
        Modelica.Blocks.Interfaces.RealInput glucoseConcentration "mg/ml" annotation (
           Placement(transformation(extent={{176,-30},{216,10}}), iconTransformation(
                extent={{-140,-38},{-100,2}})));
        Modelica.Blocks.Interfaces.RealOutput glucoseInsulinDependentUtilisation
          "mg/h" annotation (Placement(transformation(extent={{182,-10},{202,10}}),
              iconTransformation(
              extent={{-20,-20},{20,20}},
              rotation=180,
              origin={-120,30})));
        Modelica.Blocks.Interfaces.RealInput insulinConcentration "mU/ml" annotation (
           Placement(transformation(extent={{176,-30},{216,10}}), iconTransformation(
                extent={{-138,-94},{-98,-54}})));

        parameter Real tissueInsulinSensitivity=139000 "mU/h";

      equation
        glucoseInsulinDependentUtilisation = tissueInsulinSensitivity*
          insulinConcentration*glucoseConcentration;
        annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
              Bitmap(extent={{-58,-68},{96,176}}, fileName="modelica://GlucoseInsulin/images.jpg"),
              Bitmap(extent={{-100,-120},{28,48}}, fileName="modelica://GlucoseInsulin/tukova-tkan-tukove-bunky-adipocyty-vektorove-ilustrace-400-67212381.jpg"),
              Bitmap(extent={{12,-114},{100,-6}}, fileName="modelica://GlucoseInsulin/játra.png"),
              Text(
                extent={{-196,-118},{202,-138}},
                textColor={28,108,200},
                textString="%name")}),
            Diagram(coordinateSystem(preserveAspectRatio=false)));
      end InsulinDependentGlucoseUtilisation;

      model InsulinIndependentGlucoseUtilisation
        Modelica.Blocks.Interfaces.RealInput glucoseConcentration "mg/ml"
          annotation (Placement(transformation(extent={{176,52},{216,92}}),
              iconTransformation(extent={{-138,-48},{-98,-8}})));
        Modelica.Blocks.Interfaces.RealOutput
          glucoseInsulinIndependentUtilisation "mg/h" annotation (Placement(
              transformation(extent={{268,60},{288,80}}), iconTransformation(
              extent={{-18,-18},{18,18}},
              rotation=180,
              origin={-124,28})));
        parameter Real glucoseClearance=2470 "ml/h";
      equation
        glucoseInsulinIndependentUtilisation = glucoseConcentration*
          glucoseClearance;

        annotation (
          Placement(transformation(extent={{186,52},{206,72}}),
              iconTransformation(
              extent={{-20,-20},{20,20}},
              rotation=180,
              origin={-120,24})),
          Icon(coordinateSystem(preserveAspectRatio=false), graphics={Bitmap(
                  extent={{-104,-90},{94,110}}, fileName=
                    "modelica://GlucoseInsulin/brain.png"), Text(
                extent={{-296,-116},{288,-134}},
                textColor={28,108,200},
                textString="%name")}),
          Diagram(coordinateSystem(preserveAspectRatio=false)));
      end InsulinIndependentGlucoseUtilisation;

      model PancreaticInsulinProduction
        Modelica.Blocks.Interfaces.RealInput glucoseConcentration "mg/ml" annotation (
           Placement(transformation(extent={{174,2},{214,42}}), iconTransformation(
              extent={{-20,-20},{20,20}},
              rotation=270,
              origin={14,52})));
        Modelica.Blocks.Interfaces.RealOutput insulinProductionRate "mU/h"
          annotation (Placement(transformation(extent={{184,14},{204,36}}),
              iconTransformation(extent={{80,6},{122,48}})));
        parameter Real glucoseConentrationThreshold=0.51 "mg/ml";
        parameter Real insulinProductionSensitivity=1430 "mu*ml/mg/h";
      equation
        if (glucoseConcentration < glucoseConentrationThreshold) then
          insulinProductionRate = 0;
        else
          insulinProductionRate = insulinProductionSensitivity*(glucoseConcentration -
            glucoseConentrationThreshold);
        end if;
        annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
                Bitmap(extent={{-100,-100},{102,100}}, fileName="modelica://GlucoseInsulin/pancreas.jpg"), Text(
                extent={{-184,-66},{164,-82}},
                textColor={28,108,200},
                textString="%name")}),
            Diagram(coordinateSystem(preserveAspectRatio=false)));
      end PancreaticInsulinProduction;

      model InsulinDestruction
        Modelica.Blocks.Interfaces.RealOutput insulinDestructionRate "mU/h" annotation (
            Placement(transformation(extent={{184,26},{204,46}}), iconTransformation(
              extent={{-15,-15},{15,15}},
              rotation=180,
              origin={-115,1})));
        Modelica.Blocks.Interfaces.RealInput insulinConcentration "mU/ml" annotation (
           Placement(transformation(extent={{176,2},{216,42}}), iconTransformation(
                extent={{-132,-64},{-96,-28}})));
        parameter Real insulinClearance = 7600 "ml/h"
        annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
                Bitmap(extent={{-100,-100},{98,100}}, fileName="modelica://GlucoseInsulin/játra.png")}),
            Diagram(coordinateSystem(preserveAspectRatio=false)));
      equation
        insulinClearance*insulinConcentration = insulinDestructionRate;

        annotation (Icon(graphics={Bitmap(extent={{-100,-100},{98,100}}, fileName="modelica://GlucoseInsulin/játra.png"), Text(
                extent={{-188,-84},{192,-102}},
                textColor={28,108,200},
                textString="%name")}));
      end InsulinDestruction;
    end Components;

    package Tests
      model test
        Components.GlucoseInputFlowRate glucoseInputFlowRate(glucoseInput=8400)
          annotation (Placement(transformation(extent={{-88,22},{-44,54}})));
        Components.GlucoseCompartment glucose
          annotation (Placement(transformation(extent={{-30,26},{-6,50}})));
        Components.InsulinCompartment insulin
          annotation (Placement(transformation(extent={{-28,-56},{-6,-30}})));
        Components.ECT_Volume eCT_Volume(ECTvolume=15000)
                                                         "ml"
          annotation (Placement(transformation(extent={{-92,-10},{-66,4}})));
        Components.RenalLoss renalLoss
          annotation (Placement(transformation(extent={{48,56},{68,76}})));
        Components.InsulinIndependentGlucoseUtilisation
          insulinIndependentGlucoseUtilisation
          annotation (Placement(transformation(extent={{50,26},{70,46}})));
        Components.InsulinDependentGlucoseUtilisation
          insulinDependentGlucoseUtilisation
          annotation (Placement(transformation(extent={{52,-2},{72,18}})));
        Components.InsulinDestruction insulinDestruction
          annotation (Placement(transformation(extent={{8,-54},{30,-32}})));
        Components.PancreaticInsulinProduction pancreaticInsulinProduction
          annotation (Placement(transformation(extent={{-94,-58},{-60,-30}})));
      equation
        connect(glucoseInputFlowRate.glucoseInflow, glucose.glucoseInflow)
          annotation (Line(
            points={{-53.68,37.2},{-42.62,37.2},{-42.62,38.12},{-31.56,38.12}},
            color={0,0,127},
            smooth=Smooth.Bezier));

        connect(eCT_Volume.volume, glucose.volume) annotation (Line(points={{
                -65.22,-3.35},{-52,-3.35},{-52,16},{-90,16},{-90,64},{-18.24,64},
                {-18.24,50.24}}, color={0,0,127}));
        connect(insulin.volume, glucose.volume) annotation (Line(points={{
                -17.22,-29.48},{-17.22,-4},{-52,-4},{-52,16},{-90,16},{-90,64},
                {-18.24,64},{-18.24,50.24}}, color={0,0,127}));
        connect(glucose.glucoseConcentration,
          insulinDependentGlucoseUtilisation.glucoseConcentration) annotation (
            Line(points={{-12.24,24.8},{-12.24,6.2},{50,6.2}}, color={0,0,127}));
        connect(insulinIndependentGlucoseUtilisation.glucoseConcentration,
          insulinDependentGlucoseUtilisation.glucoseConcentration) annotation (
            Line(points={{48.2,33.2},{20,33.2},{20,6.2},{50,6.2}}, color={0,0,
                127}));
        connect(renalLoss.glucoseConcentration,
          insulinDependentGlucoseUtilisation.glucoseConcentration) annotation (
            Line(points={{47,69.2},{20,69.2},{20,6.2},{50,6.2}}, color={0,0,127}));
        connect(renalLoss.renalLoss, glucose.renalOutflow) annotation (Line(
              points={{46.6,64},{14,64},{14,45.2},{-4.8,45.2}}, color={0,0,127}));
        connect(glucose.insulinIndependentOutflow,
          insulinIndependentGlucoseUtilisation.glucoseInsulinIndependentUtilisation)
          annotation (Line(points={{-4.8,38},{26,38},{26,38.8},{47.6,38.8}},
              color={0,0,127}));
        connect(insulinDependentGlucoseUtilisation.glucoseInsulinDependentUtilisation,
          glucose.insulinDependentOutflow) annotation (Line(points={{50,11},{14,
                11},{14,30.08},{-4.8,30.08}}, color={0,0,127}));
        connect(insulin.insulinDestructionRate, insulinDestruction.insulinDestructionRate)
          annotation (Line(points={{-4.57,-42.87},{0.89,-42.87},{0.89,-42.89},{
                6.35,-42.89}}, color={0,0,127}));
        connect(pancreaticInsulinProduction.insulinProductionRate, insulin.insulinInflow)
          annotation (Line(points={{-59.83,-40.22},{-54,-40.22},{-54,-43.13},{
                -29.87,-43.13}}, color={0,0,127}));
        connect(pancreaticInsulinProduction.glucoseConcentration,
          insulinDependentGlucoseUtilisation.glucoseConcentration) annotation (
            Line(points={{-74.62,-36.72},{-74.62,-20},{20,-20},{20,6.2},{50,6.2}},
              color={0,0,127}));
        connect(insulinDestruction.insulinConcentration,
          insulinDependentGlucoseUtilisation.insulinConcentration) annotation (
            Line(points={{6.46,-48.06},{-2,-48.06},{-2,-62},{42,-62},{42,0.6},{
                50.2,0.6}}, color={0,0,127}));
        connect(insulin.insulinConcentration,
          insulinDependentGlucoseUtilisation.insulinConcentration) annotation (
            Line(points={{-11.72,-57.3},{-11.72,-62},{42,-62},{42,0.6},{50.2,
                0.6}}, color={0,0,127}));
        annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
              coordinateSystem(preserveAspectRatio=false)));
      end test;

      model CompartmentTest
        Components.GlucoseInputFlowRate glucoseInputFlowRate(glucoseInput=8400)
          annotation (Placement(transformation(extent={{-88,22},{-44,54}})));
        Components.GlucoseCompartment glucose
          annotation (Placement(transformation(extent={{-30,26},{-6,50}})));
        Components.InsulinCompartment insulin
          annotation (Placement(transformation(extent={{-28,-56},{-6,-30}})));
        Components.ECT_Volume eCT_Volume(ECTvolume=15000)
                                                         "ml"
          annotation (Placement(transformation(extent={{-92,-10},{-66,4}})));
        Components.RenalLoss renalLoss
          annotation (Placement(transformation(extent={{48,56},{68,76}})));
        Components.InsulinIndependentGlucoseUtilisation
          insulinIndependentGlucoseUtilisation
          annotation (Placement(transformation(extent={{50,26},{70,46}})));
        Components.InsulinDependentGlucoseUtilisation
          insulinDependentGlucoseUtilisation
          annotation (Placement(transformation(extent={{52,-2},{72,18}})));
        Components.InsulinDestruction insulinDestruction
          annotation (Placement(transformation(extent={{8,-54},{30,-32}})));
        Components.PancreaticInsulinProduction pancreaticInsulinProduction
          annotation (Placement(transformation(extent={{-94,-58},{-60,-30}})));
        Modelica.Blocks.Sources.Constant glucoseConcentration(k=0.811)
          annotation (Placement(transformation(extent={{-46,2},{-32,16}})));
        Modelica.Blocks.Sources.Constant insulinCOncentration(k=0.0567)
          annotation (Placement(transformation(extent={{-56,-90},{-42,-76}})));
      equation
        connect(glucoseInputFlowRate.glucoseInflow, glucose.glucoseInflow)
          annotation (Line(
            points={{-53.68,37.2},{-42.62,37.2},{-42.62,38.12},{-31.56,38.12}},
            color={0,0,127},
            smooth=Smooth.Bezier));

        connect(eCT_Volume.volume, glucose.volume) annotation (Line(points={{
                -65.22,-3.35},{-52,-3.35},{-52,16},{-90,16},{-90,64},{-18.24,64},
                {-18.24,50.24}}, color={0,0,127}));
        connect(insulin.volume, glucose.volume) annotation (Line(points={{
                -17.22,-29.48},{-17.22,-4},{-52,-4},{-52,16},{-90,16},{-90,64},
                {-18.24,64},{-18.24,50.24}}, color={0,0,127}));
        connect(insulinIndependentGlucoseUtilisation.glucoseConcentration,
          insulinDependentGlucoseUtilisation.glucoseConcentration) annotation (
            Line(points={{48.2,33.2},{20,33.2},{20,6.2},{50,6.2}}, color={0,0,
                127}));
        connect(renalLoss.glucoseConcentration,
          insulinDependentGlucoseUtilisation.glucoseConcentration) annotation (
            Line(points={{47,69.2},{20,69.2},{20,6.2},{50,6.2}}, color={0,0,127}));
        connect(renalLoss.renalLoss, glucose.renalOutflow) annotation (Line(
              points={{46.6,64},{14,64},{14,45.2},{-4.8,45.2}}, color={0,0,127}));
        connect(glucose.insulinIndependentOutflow,
          insulinIndependentGlucoseUtilisation.glucoseInsulinIndependentUtilisation)
          annotation (Line(points={{-4.8,38},{26,38},{26,38.8},{47.6,38.8}},
              color={0,0,127}));
        connect(insulinDependentGlucoseUtilisation.glucoseInsulinDependentUtilisation,
          glucose.insulinDependentOutflow) annotation (Line(points={{50,11},{14,
                11},{14,30.08},{-4.8,30.08}}, color={0,0,127}));
        connect(insulin.insulinDestructionRate, insulinDestruction.insulinDestructionRate)
          annotation (Line(points={{-4.57,-42.87},{0.89,-42.87},{0.89,-42.89},{
                6.35,-42.89}}, color={0,0,127}));
        connect(pancreaticInsulinProduction.insulinProductionRate, insulin.insulinInflow)
          annotation (Line(points={{-59.83,-40.22},{-54,-40.22},{-54,-43.13},{
                -29.87,-43.13}}, color={0,0,127}));
        connect(pancreaticInsulinProduction.glucoseConcentration,
          insulinDependentGlucoseUtilisation.glucoseConcentration) annotation (
            Line(points={{-74.62,-36.72},{-74.62,-20},{20,-20},{20,6.2},{50,6.2}},
              color={0,0,127}));
        connect(insulinDestruction.insulinConcentration,
          insulinDependentGlucoseUtilisation.insulinConcentration) annotation (
            Line(points={{6.46,-48.06},{-2,-48.06},{-2,-62},{42,-62},{42,0.6},{
                50.2,0.6}}, color={0,0,127}));
        connect(insulin.insulinConcentration,
          insulinDependentGlucoseUtilisation.insulinConcentration) annotation (
            Line(points={{-11.72,-57.3},{-11.72,-62},{42,-62},{42,0.6},{50.2,
                0.6}}, color={0,0,127}));
        connect(glucose.glucoseConcentration,
          insulinDependentGlucoseUtilisation.glucoseConcentration) annotation (
            Line(points={{-12.24,24.8},{-12.24,6},{20,6},{20,6.2},{50,6.2}},
              color={0,0,127}));
        annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
              coordinateSystem(preserveAspectRatio=false)));
      end CompartmentTest;
    end Tests;

  end CompartmentApproach;
  annotation (uses(Modelica(version="4.0.0")));
end GlucoseInsulin;
