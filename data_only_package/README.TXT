HISTDATA.TXT contains the complete historical database of the FRB/US model in csv format.
The historical values of the BBB corporate bond yield, RBBB, are placeholders given by
the identity RBBB = RG10 + RBBBP, where RG10 is the 10-year treasury yield and RBBBP
represents the BBB risk premium which is assumed to be 170 basis points over the whole sample.
Other data sources for BBB bond rates can be used in the model if the historical values for
RBBB and RBBBP are redefined accordingly.

FRB/US variable definitions can be found in the frbus_package .zip file, under mods/model.xml.

As noted in the April 2014 FEDS Note "The FRB/US Model: A Tool for Macroeconomic Policy Analysis,"
the baseline projection in the Eviews database follows the median projections in FOMC's Summary of
Economic Projections (SEP) where they are available. Beyond the horizon of the SEP, the baseline
projection is a model-guided extrapolation. Variables that are available in the SEP gradually
converge to the median of the SEP's longer-run projections. The funds rate mechanically follows
the predictions of the model's inertial Taylor rule beyond the horizon for which SEP projections
are available, conditional on the output gap and inflation rate in the baseline projection.
