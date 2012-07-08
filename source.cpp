#include <stdlib.h>
#include <libpq-fe.h>
#include <iostream>
#define NMAX 60 //nr. max. elem matrice
using namespace std;
double b[NMAX][NMAX]; //pentru inversa

static void iesire(PGconn *conexiune1, PGresult *rezultat)
{ fprintf(stderr, "EROARE %s", PQerrorMessage(conexiune1));
  if(rezultat) PQclear(rezultat); PQfinish(conexiune1);  exit(1);
 }
 
class MATR {
	public:
	double M[NMAX][NMAX];
	//functie afisare matrice
	void afis(int n,int m, double x[NMAX][NMAX]) {
		for(int i=0;i<n;i++)
			{for(int j=0;j<m;j++)
				cout<<x[i][j]<<" ";
			cout<<endl;
			}
	cout<<endl;
	}	
	
//functie calcul determinant (recursiv)
int cdet(int n, double M[NMAX][NMAX]) {
	int i,j,k,det=0,detA,c1,c2;
	double A[NMAX][NMAX];
	if(n==1) det=M[0][0];
	else if(n==2) det=M[0][0]*M[1][1]-M[0][1]*M[1][0];
		else {
		for(i=0;i<n;i++) { //i=coloana (prima linie fiecare coloana)
			c1=0;c2=0;
			//construim minorul
			for(j=0;j<n;j++) //linie
				for(k=0;k<n;k++) //coloana
					if(j!=0&&k!=i) {
						A[c1][c2]=M[j][k];
						c2++;
						if(c2==n-1) {
							c1++;
							c2=0;
						}
					}
		
		//calculam minorul (determinantul)
		detA=cdet(n-1,A);
		//calculez complementul algebric
		if(i%2==0) det+=(detA*M[0][i]);
		else det-=(detA*M[0][i]);
	}
	}
	return det;
}

//functie generare inversa (se foloseste de functie calcul determinant)
void inversa(int m, double c[NMAX][NMAX]) {
	int j,k,detA,i,l,c1,c2;
	double A[NMAX][NMAX],det;
	det=cdet(m,c);
	det=1/det;
	for(i=0;i<m;i++) 
		for(l=0;l<m;l++) { 
			c1=0;c2=0;
			//construim minorul
			for(j=0;j<m;j++) //linie
				for(k=0;k<m;k++) //coloana
					if(j!=i&&k!=l) {
						A[c1][c2]=c[j][k];
						c2++;
						if(c2==m-1) {
							c1++;
							c2=0;
						}
					}
		
		//calculam minorul (determinantul)
		detA=cdet(m-1,A);
		//calculez complementul algebric
		if((i+l)%2==0) b[i][l]=detA*det;
		else b[i][l]=(-1)*detA*det;
	}
}

};
 
int main()
{
	int n,m;
	double x[NMAX][NMAX],xt[NMAX][NMAX],y[NMAX],w[NMAX],aux=0,z[NMAX];
	MATR xtx;
	char *sir;
	//declarare variabile pt db
	const char *sir_conexiune;
    PGconn     *conexiune1;
    PGresult   *rezultat;
    int         nrCampuri,nrInregistrari;
//informatii db	
   sir_conexiune = "dbname = postgres password = postgres user = postgres";
//conectarea la baza de date 
  conexiune1 = PQconnectdb(sir_conexiune);
//afisare eroare daca nu se conecteaza
    if (PQstatus(conexiune1) != CONNECTION_OK) iesire(conexiune1,NULL);
//selectare tabel si preluare nr. campuri si nr. inregistrari    
    rezultat = PQexec(conexiune1, "SELECT * FROM date.ex1;");
    if (PQresultStatus(rezultat) != PGRES_TUPLES_OK) iesire(conexiune1,rezultat);
    nrCampuri = PQnfields(rezultat);
	nrInregistrari=PQntuples(rezultat);
//preia inregistrarile intr-o matrice
	n=nrInregistrari; m=nrCampuri;
    for (int i=0; i<nrInregistrari; i++) {
		x[i][0]=1;
		for (int j=0; j<nrCampuri-1; j++) {
			sir=PQgetvalue(rezultat, i, j);
			x[i][j+1]=atof(sir);
		}
		int j=nrCampuri-1;
		sir=PQgetvalue(rezultat, i, j);
		y[i]=atof(sir);
    }
//stergere informatii tabel
    PQclear(rezultat);  
//inchidere conexiune db
	PQfinish(conexiune1);
	//transpusa:
	for(int i=0;i<m;i++)
		for(int j=0;j<n;j++)
			xt[i][j]=x[j][i];
	
	//produsul:
	for(int i=0;i<m;i++)
		for(int j=0;j<m;j++)
		{for(int k=0;k<n;k++)
			aux+=(xt[i][k]*x[k][j]);
		xtx.M[i][j]=aux;
		aux=0;
		}

	//inversa
	xtx.inversa(m,xtx.M);
	
	//produsul 2:
	for(int i=0;i<m;i++)
		{w[i]=0;
		for(int k=0;k<n;k++)
			w[i]+=(xt[i][k]*y[k]);
		}
	
	//produsul final:
	for(int i=0;i<m;i++)
		{for(int k=0;k<m;k++)
			aux+=(b[i][k]*w[k]);
		z[i]=aux;
		aux=0;
		}
		
	cout<<"Ecuatia de regresie: "<<fixed<<z[0]; for(int i=1;i<m;i++) cout<<" + "<<z[i]<<" * b"<<i;
		
	//calcul R2:
	double r2,my=0,ye[NMAX],sum1=0,sum2=0;
	for(int i=0;i<n;i++) my+=y[i]; 
	my/=n; //medie y
	for(int i=0;i<n;i++) { 
		ye[i]=z[0];
		for(int j=1;j<m;j++) 
			ye[i]+=z[j]*x[i][j];
	} //vector y estimati
	for(int i=0;i<n;i++) {
		sum1+=(ye[i]-my)*(ye[i]-my);
		sum2+=(y[i]-my)*(y[i]-my);
	} 
	r2=sum1/sum2;
	cout<<endl<<"R patrat: "<<r2<<endl<<endl;
	
	system("PAUSE");
	return 0;
}