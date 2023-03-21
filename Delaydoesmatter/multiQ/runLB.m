sname=input('Enter subject code (**NO MORE THAN 4 CHARACTERS!!**: ', 's');

SubjInfofileid=fopen('MultiAdultExptSubjectInfo','at'); % 'at' is append, text mode

basicinfo=basicdialog;

for lines=1:size(basicinfo)
    fprintf(SubjInfofileid,[sname, '\t', char(basicinfo(lines)), '\n']);
end

langinfo=LanguageBackgroundBDS;

for lineses=1:size(langinfo)
    fprintf(SubjInfofileid,[sname, '\t', char(langinfo(lineses)), '\n']);
end

musinfo=MusicBackground;

for lineses=1:size(musinfo)
    fprintf(SubjInfofileid,[sname, '\t', char(musinfo(lineses)), '\n']);
end

fclose(SubjInfofileid);

runQuestions('multiQuestions','multiadultexpts',sname)

