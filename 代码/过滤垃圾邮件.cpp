#include<iostream>
#include<io.h>
#include<vector>
#include<list>
#include<string.h>
#include<fstream>
#include <ctype.h>
#include <algorithm>
#include<math.h>
using namespace std;
vector<string>files;//�洢�ʼ��ĵ�ַ
vector<string>hamvec;//�ʱ�
vector<string>spamvec;
vector<string>testvec;
vector<vector<string> >tt;
vector<int>dx;//�����Ե��ʼ�����
vector<int>dhamvec;//�����ʼ����ʼ�����
vector<int>dspamvec;//�����ʼ����ʼ�����
int hamnum=0;
int spamnum=0;
int testnum=0;
double num=0;
#define HAMPATH  "../email/ham/"
#define SPAMPATH "../email/spam/"
#define TESTPATH "../email/test/"
void getAllFiles(string path, vector<string>& files) {
    //�ļ����
    long hFile = 0;
    files.clear();
    //�ļ���Ϣ
    struct _finddata_t fileinfo;  //�����õ��ļ���Ϣ��ȡ�ṹ
    string p;  //string�������˼��һ����ֵ����:assign()���кܶ����ذ汾
    if ((hFile = _findfirst(p.assign(path).append("\\*").c_str(),&fileinfo)) != -1) {
        do {
            if ((fileinfo.attrib & _A_SUBDIR)) {  //�Ƚ��ļ������Ƿ����ļ���
                 if (strcmp(fileinfo.name,".") != 0 && strcmp(fileinfo.name,"..") != 0) {
                     files.push_back(p.assign(path).append("\\").append(fileinfo.name));
                     getAllFiles(p.assign(path).append("\\").append(fileinfo.name), files);
                 }
             } else {
                 files.push_back(p.assign(path).append("\\").append(fileinfo.name));
             }         } while (_findnext(hFile, &fileinfo) == 0);  //Ѱ����һ�����ɹ�����0������-1
         _findclose(hFile);
     }
 }
void filter(string s,vector<string> &svec)//���˵��ո�ͱ����ţ������ʴ���svec
{
    s+=" ";
    string tmp="";
    for(int i=0;i<s.size();i++)
    {

        if(s[i]>='a'&&s[i]<='z'||s[i]>='A'&&s[i]<='Z')
        {
            tmp+=s[i];
        }
        if(tmp.size()>0&&s[i]==' '||s[i]==','||s[i]=='.'||s[i]=='!'||s[i]=='?')
        {
            if(tmp.size()>1)
            {
                svec.push_back(tmp);
            }
            tmp.clear();
        }
    }
}
 void reademail(vector<string>&svec,int flag)
 {
     if(flag==2)
     {
         spamnum=files.size();
         //cout<<"��ȡ�����ʼ�,����:"<<spamnum<<endl;
     }
     if(flag==1)
     {
         hamnum=files.size();
         //cout<<"��ȡ�����ʼ�,����"<<hamnum<<endl;
     }
     if(flag==0)
     {
         testnum=files.size();
     }
     for(int i=0;i<files.size();i++)
     {
         string s;
         fstream f(files[i]);
         if(!f.is_open())
         {
             cout<<"�ʼ���ȡʧ��!"<<endl;
             exit(1);
         }
         while(getline(f,s))
         {
             //cout<<s<<endl;
             filter(s,svec);
         }
         f.close();
     }

 }
 void buildwords(string Path,vector<string>&WordsName,int flag)//flag>1��ʾ�����ʼ����������ʼ�
 {
    getAllFiles(Path, files);
    reademail(WordsName,flag);
    sort(WordsName.begin(),WordsName.end());
    //�����ʼ��������ʼ��Ĵʻ��Ҫȥ���ظ�Ԫ�أ������Ե��ʼ�����Ҫ,
    if(flag>0)
    {
        WordsName.erase(unique(WordsName.begin(),WordsName.end()),WordsName.end());
        //cout<<WordsName.size()<<endl;
    }

 }
 bool IsExist(string s,vector<string>svec)
 {

     for(int i=0;i<svec.size();i++)
     {
         if(s==svec[i])
            return true;
     }
     return false;
 }
void BuildVectorTable(vector<string>svec,vector<string>te,vector<int>&dvec)
{
    //svec�������ʼ����ʼ�,te�Ǳ������ʼ����ʼ�,devc�����洢���ɵ�����
//    cout<<"�����ʼ����ʼ�:"<<endl;
//    for(int i=0;i<svec.size();i++)
//    {
//        cout<<svec[i]<<" ";
//    }
//    cout<<"\n�������ʼ����ʼ�:\n";
//    for(int i=0;i<testvec.size();i++)
//    {
//        cout<<testvec[i]<<" ";
//    }
    dvec.clear();
    for(int i=0;i<svec.size();i++)
    {
        if(IsExist(svec[i],te))
            dvec.push_back(1);
        else
            dvec.push_back(0);
    }
}
double CosineSimilarityCalculation(vector<int>ivec1,vector<int>ivec2)
{
    double absolute_a=0,absolute_b=0,product_ab=0;
    if(ivec1.size()!=ivec2.size())
    {
        cout<<"�������ƶȼ������!";
        exit(1);
    }
    for(int i=0;i<ivec1.size();i++)
    {
        absolute_a+=ivec1[i]*ivec1[i];
        absolute_b+=ivec2[i]*ivec2[i];
        product_ab+=ivec1[i]*ivec2[i];
    }
    if(absolute_a==0||absolute_b==0)
        return 0;
    //cout<<"a*b:"<<product_ab<<endl;
    //cout<<"||a||:"<<absolute_a<<" ||b||:"<<absolute_b<<endl;
    double cos_ans=product_ab/(sqrt(absolute_a)*sqrt(absolute_b));
    return cos_ans;
}
void Bayes()
{
    /*p(c|x)=p(x|c)*p(c)/p(x),����px=p(x|c1)+p(x|c2)��c1�Ǵ��������ʼ�
    ��c2���������ʼ��������p(x|c),��dhamvec��(1......1)����������*/
    //������������
    vector<int>globe;
    globe.clear();
    for(int i=0;i<dhamvec.size();i++)
    {
        globe.push_back(1);//����ȫ������
    }
    double px=0,pcx0=0,pcx1=0;
    //cout<<"�����ʼ�:"<<hamnum<<",�����ʼ�:"<<spamnum<<";�����ʼ�"<<hamnum+spamnum<<endl;
    double pham=hamnum/(double)(hamnum+spamnum);
    //cout<<"�����ʼ����������:"<<pham<<endl;
    double pspam=spamnum/(double)(hamnum+spamnum);
    //cout<<"�����ʼ����������:"<<pspam<<endl;
    pcx0=CosineSimilarityCalculation(dhamvec,globe);
    //cout<<"pxc0:"<<pxc0<<endl;
    globe.clear();
    for(int i=0;i<dspamvec.size();i++)
        globe.push_back(1);
    pcx1=CosineSimilarityCalculation(dspamvec,globe);
    //cout<<"pxc1:"<<pxc1<<endl;
    double ham_ans,spam_ans;
    if(pcx0==0)
    {
        //�����ʼ��������ʼ������ƶ�Ϊ0��ֱ���ж���Ϊ�����ʼ�
        ham_ans=0;
        spam_ans=1;
    }
    else if(pcx1==0)
    {
        //�����ʼ��������ʼ������ƶ�Ϊ0��ֱ���ж���Ϊ�����ʼ�
        ham_ans=1;
        spam_ans=0;
    }
    else
    {
        px=pcx0+pcx1;
        ham_ans=pcx0*pham/px;
        spam_ans=pcx1*pspam/px;
    }
    cout<<"�����ʼ��������ʼ��ĸ�����:"<<ham_ans<<endl;
    cout<<"�����ʼ��������ʼ��ĸ�����:"<<spam_ans<<endl;
    if(ham_ans>=spam_ans)
    {
        cout<<"���ʼ��������ʼ�!"<<endl;
    }
    else
    {

        cout<<"���ʼ��������ʼ�!"<<endl;
        num++;
    }
}
void ReadTestWords(string path)
{
    vector<string>spath;
    vector<string>stmp;
    getAllFiles(path,spath);
    string s;
    for(int i=0;i<spath.size();i++)
    {
        //cout<<"��"<<i+1<<"���ʼ�!"<<endl;
        //cout<<spath[i]<<endl;
        fstream f(spath[i]);
        if(!f.is_open())
         {
             cout<<"�ʼ���ȡʧ��!"<<endl;
             exit(1);
         }
         while(getline(f,s))
         {
             filter(s,stmp);
         }
         tt.push_back(stmp);
         stmp.clear();
    }
}
int main()
{
    //���������ʼ��Ĵʻ��
    buildwords(HAMPATH,hamvec,1);
    //���������ʼ��Ĵʻ��
    buildwords(SPAMPATH,spamvec,2);
    //�����������ʼ��Ĵʻ��
    ReadTestWords(TESTPATH);
//    for(int i=0;i<testvec.size();i++)
//    {
//        cout<<testvec[i]<<" ";
//    }
    /*���������ݱ��൥�ʼ���ĵ����Ƿ��ڵ��ʱ��г��ֹ�������������Ϊ1��û����Ϊ0
    ��ʽ:p(c|x)=p(x|c)*p(c)/p(x),����p(x)չ����ȫ���ʹ�ʽ������*/
    cout<<"�����ʼ���:"<<hamnum<<"�⣬�����ʼ���:"<<spamnum<<endl;
    for(int i=0;i<tt.size();i++)
    {
        BuildVectorTable(hamvec,tt[i],dhamvec);
        BuildVectorTable(spamvec,tt[i],dspamvec);
        //��ÿ���ʼ��ñ�Ҷ˹�㷨����
        Bayes();
    }
    cout<<"���ж�:"<<tt.size()<<"��;����:"<<num<<"��;"<<"׼ȷ��:"<<(tt.size()-num)/tt.size()<<endl;


}
