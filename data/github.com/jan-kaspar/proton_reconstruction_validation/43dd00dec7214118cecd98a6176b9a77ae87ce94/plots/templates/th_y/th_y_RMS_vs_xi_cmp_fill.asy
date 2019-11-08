import root;
import pad_layout;
include "../settings.asy";

string topDir = "../../../";

TGraph_errorBar = None;

xTicksDef = LeftTicks(0.05, 0.01);

//----------------------------------------------------------------------------------------------------

NewPad(false);
AddToLegend("year: " + year);
AddToLegend("version: " + version);
AddToLegend("stream: " + stream);
AddToLegend("xangle: " + xangle);
AddToLegend("beta: " + beta);
AttachLegend();

//----------------------------------------------------------------------------------------------------

for (int ai : arms.keys)
	NewPadLabel(a_labels[ai]);

for (int fi : fills_short.keys)
{
	string fill = fills_short[fi];

	NewRow();

	NewPadLabel("fill: " + fill);

	for (int ai : arms.keys)
	{
		NewPad("$\xi_{\rm multi}$", "RMS of $\th^*_y\ung{\mu rad}$");

		string f = topDir + "data/" + year + "/" + version + "/fill_" + fill + "/xangle_" + GetXangle(fill, xangle)
			+ "_beta_" + GetBeta(fill) + "_stream_" + stream + "/do_fits.root";
		string on = "multiRPPlots/" + arms[ai] + "/g_th_y_RMS_vs_xi";

		RootObject hist = RootGetObject(f, on, error=false);
		RootObject fit = RootGetObject(f, on + "|ff_pol1", error=false);
		if (!hist.valid)
			continue;

		draw(scale(1., 1e6), hist, "p", blue, mCi+1pt+blue);
		draw(scale(1., 1e6), fit, "def", red+1pt);

		limits((0.00, 0), (0.25, +100), Crop);
	}
}

GShipout(hSkip=0mm, vSkip=0mm);
