# amjobs
Reads CPS Merged Outgoing Rotation Groups (MORG) files and replicates the findings of the study "Immigration and American Jobs" by Madeline Zavodny.
<pre>
Description of programs:

amjobs.R     - function called by following 4 amjobs*.R files
amjobs0.R    - processes author's data from 2000-2007
amjobs07.R   - processes CPS MORG data from 2000-2007
amjobs13.R   - processes CPS MORG data from 2000-2013
amjobs13lf.R - processes CPS MORG data from 2000-2013 with modified native_emprate
amjobsg.R    - various functions
morg07.R     - extracts CPS MORG data from 2000-2007
morg13.R     - extracts CPS MORG data from 2000-2013
morg13lf.R   - extracts CPS MORG data from 2000-2013 with modified native_emprate

Note: The morg*.dta files that are read by the morg*.R files can be found at <A HREF="http://nber.org/morg/annual/">http://nber.org/morg/annual/</A>

For more information, see <A HREF="http://econdataus.com/amjobs.htm">http://econdataus.com/amjobs.htm</A> 
</pre>
