#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void main(void)
{
	FILE *obs_log, *script;
	char *infilename,*name,*north, *south, *east, *west
	, *start, *stop, *date, *tsys, *scan, *endptr;
	char sep[] = ",";
	char  line[500], outfilename[100], pca[10], pb[500], pointing[6];
	int mem;

	obs_log = fopen("observations.txt", "r");
	script = fopen("reductions", "w");

	fprintf(script, "set plotdev /xw \n\n");

	while (fgets(line, 500, obs_log)!=NULL)
	{
		/* Read the values into the relevant variables */
		infilename = strtok(line, sep);
		name = strtok(NULL, sep);
		north = strtok(NULL, sep);
		south = strtok(NULL, sep);
		east = strtok(NULL, sep);
		west = strtok(NULL, sep);
		start = strtok(NULL, sep);
		stop = strtok(NULL, sep);
		date = strtok(NULL, sep);
		scan = strtok(NULL, sep);
		tsys = strtok(NULL, sep);

		strcpy(outfilename,"../");
		strcat(outfilename,infilename);
		
		/* Determine type of pointing, if it exists */
		if (strcmp(north, "0")==0)
			strcpy(pointing, "FALSE");
		else
			strcpy(pointing, "TRUE");

		/* Select the velocity blocks for the baseline fit and peak
		velocity for pointing corrections */
		strcpy(pca, "");
		strcpy(pb, "");
		if (strcmp(infilename,"sg1742.asc")==0)
		{
			strcpy(pca, "pca 1.63");
			strcpy(pb, "pb -1.04 -0.1\n pb  5 6 ");
		}
		if (strcmp(infilename,"sg2137.asc")==0)
		{
			strcpy(pca, "pca 10.9");
			strcpy(pb, "pb 7.4 9.8\n pb 13.6 4.4");
		}
		if (strcmp(infilename,"sg1890.asc")==0)
		{
			strcpy(pca, "pca 8.88");
			strcpy(pb, "pb 3.9 7.9\n pb 12.4 19.9");
		}
		if (strcmp(infilename,"sg1889.asc")==0)
		{
			strcpy(pca, "pca 10.5");
			strcpy(pb, "pb 6.5 7.9\n pb 11.91 13.5");
		}
		if (strcmp(infilename,"sg1926.asc")==0)
		{
			strcpy(pca, " ");
			strcpy(pb, "pb 0 1.4   \n pb 6 7         ");
		}
		if (strcmp(infilename,"sg1964.asc")==0)
		{
			strcpy(pca, " ");
			strcpy(pb, "pb 10.46 13.48\n pb  16.86 17.54");
		}
		if (strcmp(infilename,"sg2873.asc")==0)
		{
			strcpy(pca, "pca -1.9 ");
			strcpy(pb, "pb -5.56 -3.25  \n pb -0.1 1.5    ");
		}
		if (strcmp(infilename,"sg2912.asc")==0)
		{
			strcpy(pca, "pca -29.64 ");
			strcpy(pb, "pb -34.1 -31.27     \n pb -28.16 -19.91    ");
		}
		if (strcmp(infilename,"sg2945.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb -14.56 -13.7     \n pb  -9 -8");
		}
		if (strcmp(infilename,"sg2982.asc")==0)
		{
			strcpy(pca, "pca -30");
			strcpy(pb, "pb -33.05 -31.25     \n pb -28.85 -26.03    ");
		}
		if (strcmp(infilename,"sg3052.asc")==0)
		{
			strcpy(pca, "pca -38.4 ");
			strcpy(pb, "pb -43.46 -40.33 \n pb -33.99 -30.93 ");
		}
		if (strcmp(infilename,"sg3089.asc")==0)
		{
			strcpy(pca, " ");
			strcpy(pb, "pb -57.53 -56.05    \n pb -52.08 -50.50    ");
		}
		if (strcmp(infilename,"sg3099.asc")==0)
		{
			strcpy(pca, "pca -59.77");
			strcpy(pb, "pb -66.16 -63.8     \n pb  -57.09 -52.02   ");
		}
		if (strcmp(infilename,"sg3121.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb -52.76 -52.13    \n pb -48.12 -46.88    ");
		}
		if (strcmp(infilename,"sg3166.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb -26.64 -25.15    \n pb -15.38 -12.50    ");
		}
		if (strcmp(infilename,"sg3189.asc")==0)
		{
			strcpy(pca, "pca -34.68");
			strcpy(pb, "pb -41.99 -38.71    \n pb -31.12 -27.80    ");
		}
		if (strcmp(infilename,"sg3202.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb -68.22 -65.13   ");
		}
		if (strcmp(infilename,"sg3221.asc")==0)
		{
			strcpy(pca, "pca -63.29");
			strcpy(pb, "pb -65.62 -65.20    \n pb -52.25 -51.58    ");
		}
		if (strcmp(infilename,"sg3237.asc")==0)
		{
			strcpy(pca, "pca -51.08");
			strcpy(pb, "pb -52.76 -52.26    \n pb -47.34 -46.55    ");
		}
		if (strcmp(infilename,"sg3288.asc")==0)
		{
			strcpy(pca, "pca -43.87");
			strcpy(pb, "pb -47.54 -47.19    \n pb  -42.76  -40.54  ");
		}
		if (strcmp(infilename,"sg3282.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb -53.07 -50.57    \n pb -31.61  -25.2    ");
		}
		if (strcmp(infilename,"sg3311.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb -89.20 -87.69    \n pb -80.73 -79.50    ");
		}
		if (strcmp(infilename,"sg3312.asc")==0)
		{
			strcpy(pca, "pca -81.8 ");
			strcpy(pb, "pb -89.05 -86.36     \n pb  -76.95 -74.34       ");
		}
		if (strcmp(infilename,"sg3355.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb -121.60 -119.10  \n pb -109.9 -107.51 ");
		}
		if (strcmp(infilename,"sg3360.asc")==0)
		{
			strcpy(pca, "pca -53.43");
			strcpy(pb, "pb -61.07 -54.88    \n pb -38.68 -32.87    ");
		}
		if (strcmp(infilename,"sg3369.asc")==0)
		{
			strcpy(pca, " ");
			strcpy(pb, "pb -128.55 -127.21  \n pb -116.77 -114.58  ");
		}
		if (strcmp(infilename,"sg3379.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb  -41.47 -39.79   \n pb -35.57 -34.51   ");
		}
		if (strcmp(infilename,"sg3389.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb -46.45 -43.61    \n pb -39.91 -39.57    ");
		}
		if (strcmp(infilename,"sg3396.asc")==0)
		{
			strcpy(pca, "pca -35.82");
			strcpy(pb, "pb -41.54 -39.72    \n pb -29.38 -27.73    ");
		}
		if (strcmp(infilename,"sg3407.asc")==0)
		{
			strcpy(pca, " ");
			strcpy(pb, "pb -114.25 -110.6   \n pb -88.12 -85.78    ");
		}
		if (strcmp(infilename,"sg3398.asc")==0)
		{
			strcpy(pca, "pca -38.81");
			strcpy(pb, "pb -43.18 -40.52    \n pb -31.43 -29.14    ");
		}
		if (strcmp(infilename,"sg3442.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb -38.78 -34.07    \n pb -17.45 -10.33    ");
		}
		if (strcmp(infilename,"sg3455.asc")==0)
		{
			strcpy(pca, "pca -17.75");
			strcpy(pb, "pb -24.08 -23.17   \n pb -10.67 -9.99     ");
		}
		if (strcmp(infilename,"sg3450.asc")==0)
		{
			strcpy(pca, "pca -22.54");
			strcpy(pb, "pb -33.19 -32.30   \n pb -20.82 -18.99    ");
		}
		if (strcmp(infilename,"sg3485.asc")==0)
		{
			strcpy(pca, "pca -10.66");
			strcpy(pb, "pb -29.28 -24.95    \n pb -5.28  -0.99     ");
		}
		if (strcmp(infilename,"sg3514.asc")==0)
		{
			strcpy(pca, "pca -10.45");
			strcpy(pb, "pb  -13.12 -11.87   \n pb -6.33 -6.02    ");
		}

		if (strcmp(infilename,"ngc.asc")==0)
		{
			strcpy(pca, "pca -10.45");
			strcpy(pb, "pb -14.14 -12.27    \n pb  -1.9  -0.02     ");
		}
		if (strcmp(infilename,"sg3515.asc")==0)
		{
			strcpy(pca, " ");
			strcpy(pb, "pb -100.47 -99.78   \n pb -89.99 -84.54    ");
		}
		if (strcmp(infilename,"sg3517.asc")==0)
		{
			strcpy(pca, "pca 1.67  ");
			strcpy(pb, "pb  -2.04 0.33      \n pb  2.88 4.95       ");
		}
		if (strcmp(infilename,"sg3546.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb  -33.95 -27.83   \n pb -11.85  -5.82   ");
		}
		if (strcmp(infilename,"sg3596.asc")==0)
		{
			strcpy(pca, "pca 22.66 ");
			strcpy(pb, "pb 13.41 16.64    \n pb 25.80 27.48      ");
		}
		if (strcmp(infilename,"sg0096.asc")==0)
		{
			strcpy(pca, "pca 1.21  ");
			strcpy(pb, "pb  -4.56 -2.20     \n pb 7.25 9.67        ");
		}
		if (strcmp(infilename,"w31.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb  54.88 57.34     \n pb 79.23 83.17      ");
		}
		if (strcmp(infilename,"sg0103.asc")==0)
		{
			strcpy(pca, "pca 9.97  ");
			strcpy(pb, "pb 3.41 4.0         \n pb  16.26  17.42    ");
		}

		if (strcmp(infilename,"sg0128.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb  21.22 27.20     \n pb  42.81 49.08     ");
		}
		if (strcmp(infilename,"sgw33a.asc")==0)
		{
			strcpy(pca, "pca 57.51 ");
			strcpy(pb, "pb 48.29 49.99      \n pb 61.37  62.54     ");
		}
		if (strcmp(infilename,"sgw33b.asc")==0)
		{
			strcpy(pca, "pca 39.81");
			strcpy(pb, "pb  34.47  35.09    \n pb 41.0 41.48       ");
		}
		if (strcmp(infilename,"sg0234.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb 89.29 94.85      \n pb 109.14 117.75    ");
		}
		if (strcmp(infilename,"sg0230.asc")==0)
		{
			strcpy(pca, "pca 75.11 ");
			strcpy(pb, "pb 69.82 71.95      \n pb 83.14  84.04     ");
		}
		if (strcmp(infilename,"sg0352.asc")==0)
		{
			strcpy(pca, "pca 42.39 ");
			strcpy(pb, "pb 35.92 39.9       \n pb 46.84 49.94      ");
		}
		if (strcmp(infilename,"sg0450.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb 55.05 56.96     \n pb 59.29 61.91      ");
		}
		if (strcmp(infilename,"sg0526.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb 62.45 63.85      \n pb 67.46 69.37      ");
		}
		if (strcmp(infilename,"sg0597.asc")==0)
		{
			strcpy(pca, "");
			strcpy(pb, "pb 22.11 23.39      \n pb 28.63 31.98     ");
		}
		if (strcmp(infilename,"sgw75n.asc")==0)
		{
			strcpy(pca, "pca 7.07  ");
			strcpy(pb, "pb -0.51 2.55       \n pb 10.51 13.63      ");
		}
		if (strcmp(infilename,"sg0695.asc")==0)
		{
			strcpy(pca, "pca 14.63 ");
			strcpy(pb, "pb -6.82 -2.41      \n pb  17.58 21.8      ");
		}
		if (strcmp(infilename,"sg0757.asc")==0)
		{
			strcpy(pca, "pca  -2.76");
			strcpy(pb, "pb -18.07 -12.98    \n pb  3.43 9.53       ");
		}
		if (strcmp(infilename,"sg0781.asc")==0)
		{
			strcpy(pca, "pca -6.12 ");
			strcpy(pb, "pb -12.99 -9.33     \n pb  -4.0 0.8        ");
		}

		/*Write the reduction script*/
		fprintf(script, "clr all \n");
		fprintf(script, "pb clear \n");
		if (strcmp(pointing,"TRUE")==0)
		{
			fprintf(script, "do i %s %s 1 \n",north, start);
			if (strcmp(north, "1")!=0)
				mem = strtol(north, &endptr, 10) -1;
			else
				mem = 0;
			fprintf(script, "   ra %s i i new i-%d\n", infilename, mem);
			fprintf(script, "end\n");
			fprintf(script, "%s\n", pca);
			fprintf(script, "clr \n");
		}
		fprintf(script, "ra %s %s %s add \n", infilename, start, stop);
		fprintf(script, "mc pc\n");
		fprintf(script, "sf \n");
		fprintf(script, "%s\n", pb);
		fprintf(script, "po pb 1 \n");
		fprintf(script, "6668jy \n");
		fprintf(script, "pl\n");
		/*fprintf(script, "pause \n");*/
		fprintf(script, "wa %s\n", outfilename);
		fprintf(script,"\n");
	}
	fclose(obs_log);
	fclose(script);
}
