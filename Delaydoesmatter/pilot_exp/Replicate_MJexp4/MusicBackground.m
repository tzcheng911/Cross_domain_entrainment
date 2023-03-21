function varargout = MusicBackground(varargin)
% MUSICBACKGROUND M-file for MusicBackground.fig
%      MUSICBACKGROUND by itself, creates a new MUSICBACKGROUND or raises the
%      existing singleton*.
%
%      H = MUSICBACKGROUND returns the handle to a new MUSICBACKGROUND or the handle to
%      the existing singleton*.
%
%      MUSICBACKGROUND('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in MUSICBACKGROUND.M with the given input arguments.
%
%      MUSICBACKGROUND('Property','Value',...) creates a new MUSICBACKGROUND or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before MusicBackground_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to MusicBackground_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help MusicBackground

% Last Modified by GUIDE v2.5 12-Feb-2012 17:44:49

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @MusicBackground_OpeningFcn, ...
                   'gui_OutputFcn',  @MusicBackground_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT

% --- Executes just before MusicBackground is made visible.
function MusicBackground_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to MusicBackground (see VARARGIN)

% Choose default command line output for MusicBackground
handles.output = 'Yes';

% Update handles structure
guidata(hObject, handles);

% Insert custom Title and Text if specified by the user
% Hint: when choosing keywords, be sure they are not easily confused 
% with existing figure properties.  See the output of set(figure) for
% a list of figure properties.
if(nargin > 3)
    for index = 1:2:(nargin-3),
        if nargin-3==index, break, end
        switch lower(varargin{index})
         case 'title'
          set(hObject, 'Name', varargin{index+1});
         case 'string'
          set(handles.text1, 'String', varargin{index+1});
        end
    end
end

% Determine the position of the dialog - centered on the callback figure
% if available, else, centered on the screen
FigPos=get(0,'DefaultFigurePosition');
OldUnits = get(hObject, 'Units');
set(hObject, 'Units', 'pixels');
OldPos = get(hObject,'Position');
FigWidth = OldPos(3);
FigHeight = OldPos(4);
if isempty(gcbf)
    ScreenUnits=get(0,'Units');
    set(0,'Units','pixels');
    ScreenSize=get(0,'ScreenSize');
    set(0,'Units',ScreenUnits);

    FigPos(1)=1/2*(ScreenSize(3)-FigWidth);
    FigPos(2)=2/3*(ScreenSize(4)-FigHeight);
else
    GCBFOldUnits = get(gcbf,'Units');
    set(gcbf,'Units','pixels');
    GCBFPos = get(gcbf,'Position');
    set(gcbf,'Units',GCBFOldUnits);
    FigPos(1:2) = [(GCBFPos(1) + GCBFPos(3) / 2) - FigWidth / 2, ...
                   (GCBFPos(2) + GCBFPos(4) / 2) - FigHeight / 2];
end
FigPos(3:4)=[FigWidth FigHeight];
set(hObject, 'Position', FigPos);
set(hObject, 'Units', OldUnits);

% Show a question icon from dialogicons.mat - variables questIconData
% and questIconMap
load dialogicons.mat

IconData=questIconData;
questIconMap(256,:) = get(handles.figure1, 'Color');
IconCMap=questIconMap;

Img=image(IconData, 'Parent', handles.axes1);
set(handles.figure1, 'Colormap', IconCMap);

set(handles.axes1, ...
    'Visible', 'off', ...
    'YDir'   , 'reverse'       , ...
    'XLim'   , get(Img,'XData'), ...
    'YLim'   , get(Img,'YData')  ...
    );

% Make the GUI modal
set(handles.figure1,'WindowStyle','modal')

% UIWAIT makes MusicBackground wait for user response (see UIRESUME)
uiwait(handles.figure1);

% --- Outputs from this function are returned to the command line.
function varargout = MusicBackground_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
%varargout{1} = handles.output;
temp1 = get(handles.haveAP,'String');
temp2 = get(handles.haveAP,'Value');
APstatus=temp1(temp2);


varargout{1} = {['Recorded on: ' datestr(clock())];...
['Music appreciation: ' int2str(get(handles.musicappreciation,'Value'))];...
['Music history: ' int2str(get(handles.musichistory,'Value'))];...
['Music theory: ' int2str(get(handles.musictheory,'Value'))];...
['Music composition: ' int2str(get(handles.musiccomposition,'Value'))];...
['Music perception/cognition: ' int2str(get(handles.musicperceptioncourse,'Value'))];...
['Auditory perception/illusions: ' int2str(get(handles.auditorycourse,'Value'))];...
['Performance group as class: ' int2str(get(handles.performinggroupcourse,'Value'))];...
['No music courses: ' int2str(get(handles.nomusiccourses,'Value'))];...
['Other courses: ' get(handles.othercourses,'Value') char(get(handles.othercourseswere,'String')) ];...
['Majoring in music: ' int2str(get(handles.musicmajor,'Value'))];...
['Minoring in music: ' int2str(get(handles.musicminor,'Value'))];...
['Major instrument 1: ' char(get(handles.instrument1,'String'))];...
['Age started playing: ' char(get(handles.startage1,'String'))];...
['Age stopped playing: ' char(get(handles.endage1,'String'))];...
['Private lessons: ' int2str(get(handles.privatelessons1,'Value'))];...
['Group lessons: ' int2str(get(handles.grouplessons1,'Value'))];...
['Played in groups: ' int2str(get(handles.playgroup1,'Value'))];...
['Play currently: ' int2str(get(handles.currentplay1,'Value'))];...
['Major instrument 2: ' char(get(handles.instrument2,'String'))];...
['Age started playing: ' char(get(handles.startage2,'String'))];...
['Age stopped playing: ' char(get(handles.endage2,'String'))];...
['Private lessons: ' int2str(get(handles.privatelessons2,'Value'))];...
['Group lessons: ' int2str(get(handles.grouplessons2,'Value'))];...
['Played in groups: ' int2str(get(handles.playgroup2,'Value'))];...
['Play currently: ' int2str(get(handles.currentplay2,'Value'))];...
['Major instrument 3: ' char(get(handles.instrument3,'String'))];...
['Age started playing: ' char(get(handles.startage3,'String'))];...
['Age stopped playing: ' char(get(handles.endage3,'String'))];...
['Private lessons: ' int2str(get(handles.privatelessons3,'Value'))];...
['Group lessons: ' int2str(get(handles.grouplessons3,'Value'))];...
['Played in groups: ' int2str(get(handles.playgroup3,'Value'))];...
['Play currently: ' int2str(get(handles.currentplay3,'Value'))];...
['Other instruments played: ' char(get(handles.otherinstruments,'String'))];...
['Have AP: ' char(APstatus)];...
['Listening hours/week: ' char(get(handles.listenperweek,'String'))];...
['Playing hours/week: ' char(get(handles.playperweek,'String'))];...
['Like to compose: ' char(get(handles.liketocompose,'String'))];...
['Attend more to: ' char(get(handles.attendmoreto,'String'))];...
['Any other info: ' char(get(handles.anythingelse,'String')) ]};



% The figure can be deleted now
delete(handles.figure1);

% --- Executes on button press in pushbutton1.
function pushbutton1_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

handles.output = get(hObject,'String');

% Update handles structure
guidata(hObject, handles);

% Use UIRESUME instead of delete because the OutputFcn needs
% to get the updated handles structure...

% check if they specified AP status...  SCC 2/13/2012
temp1 = get(handles.haveAP,'String');
temp2 = get(handles.haveAP,'Value');
APstatus=temp1(temp2);


% SCC 2-13-2012: This is a catch in case people don't fill out everything
% MUST specify one set of music courses
if (get(handles.musicappreciation,'Value')+get(handles.musichistory,'Value')+...
    get(handles.musictheory,'Value')+get(handles.musiccomposition,'Value')+...
    get(handles.musicperceptioncourse,'Value')+get(handles.auditorycourse,'Value')+...
    get(handles.performinggroupcourse,'Value')+get(handles.nomusiccourses,'Value')+...
    get(handles.othercourses,'Value'))==0
    beep
% it was getting way confused by == on strings, so I used strcmp instead
% and it seems to work better--SCC
% MUST specify something for at least one instrument, and listening stuff
elseif (strcmp(char(get(handles.listenperweek,'String')),'.') || strcmp(char(get(handles.playperweek,'String')),'.') ||...
        strcmp(char(get(handles.liketocompose,'String')),'.') || strcmp(char(get(handles.attendmoreto,'String')),'.') ||...
        strcmp(char(get(handles.instrument1,'String')),'.'))
    beep
% MUST specify AP status
elseif (strcmp(APstatus,'Answer here...'))
    beep
% if an instrument is specified, must change the ages from 99 (dummy value)
elseif not(strcmp(char(get(handles.instrument1,'String')),'none')) && ...
        (strcmp(char(get(handles.startage1,'String')),'99') || strcmp(char(get(handles.endage1,'String')),'99'))
    beep
elseif not(strcmp(char(get(handles.instrument2,'String')),'.')) && ...
        (strcmp(char(get(handles.startage2,'String')),'99') || strcmp(char(get(handles.endage2,'String')),'99'))
    beep
elseif not(strcmp(char(get(handles.instrument3,'String')),'.')) && ...
        (strcmp(char(get(handles.startage3,'String')),'99') || strcmp(char(get(handles.endage3,'String')),'99'))
    beep
else
    uiresume(handles.figure1);
end

% --- Executes on button press in pushbutton2.
function pushbutton2_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

handles.output = get(hObject,'String');

% Update handles structure
guidata(hObject, handles);

% Use UIRESUME instead of delete because the OutputFcn needs
% to get the updated handles structure.
uiresume(handles.figure1);


% --- Executes when user attempts to close figure1.
function figure1_CloseRequestFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if isequal(get(handles.figure1, 'waitstatus'), 'waiting')
    % The GUI is still in UIWAIT, us UIRESUME
    uiresume(handles.figure1);
else
    % The GUI is no longer waiting, just close it
    delete(handles.figure1);
end


% --- Executes on key press over figure1 with no controls selected.
function figure1_KeyPressFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Check for "enter" or "escape"
if isequal(get(hObject,'CurrentKey'),'escape')
    % User said no by hitting escape
    handles.output = 'No';
    
    % Update handles structure
    guidata(hObject, handles);
    
    uiresume(handles.figure1);
end    
    
if isequal(get(hObject,'CurrentKey'),'return')
    uiresume(handles.figure1);
end    



function haveAP_Callback(hObject, eventdata, handles)
% hObject    handle to haveAP (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of haveAP as text
%        str2double(get(hObject,'String')) returns contents of haveAP as a double


% --- Executes during object creation, after setting all properties.
function haveAP_CreateFcn(hObject, eventdata, handles)
% hObject    handle to haveAP (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end





function otherinfo_Callback(hObject, eventdata, handles)
% hObject    handle to otherinfo (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of otherinfo as text
%        str2double(get(hObject,'String')) returns contents of otherinfo as a double


% --- Executes during object creation, after setting all properties.
function otherinfo_CreateFcn(hObject, eventdata, handles)
% hObject    handle to otherinfo (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in musicappreciation.
function musicappreciation_Callback(hObject, eventdata, handles)
% hObject    handle to musicappreciation (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of musicappreciation


% --- Executes on button press in musichistory.
function musichistory_Callback(hObject, eventdata, handles)
% hObject    handle to musichistory (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of musichistory


% --- Executes on button press in musictheory.
function musictheory_Callback(hObject, eventdata, handles)
% hObject    handle to musictheory (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of musictheory


% --- Executes on button press in checkbox4.
function checkbox4_Callback(hObject, eventdata, handles)
% hObject    handle to checkbox4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of checkbox4


% --- Executes on button press in musiccomposition.
function musiccomposition_Callback(hObject, eventdata, handles)
% hObject    handle to musiccomposition (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of musiccomposition


% --- Executes on button press in performinggroupcourse.
function performinggroupcourse_Callback(hObject, eventdata, handles)
% hObject    handle to performinggroupcourse (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of performinggroupcourse


% --- Executes on button press in nomusiccourses.
function nomusiccourses_Callback(hObject, eventdata, handles)
% hObject    handle to nomusiccourses (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of nomusiccourses


% --- Executes on button press in othercourses.
function othercourses_Callback(hObject, eventdata, handles)
% hObject    handle to othercourses (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of othercourses



function othercourseswere_Callback(hObject, eventdata, handles)
% hObject    handle to othercourseswere (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of othercourseswere as text
%        str2double(get(hObject,'String')) returns contents of othercourseswere as a double


% --- Executes during object creation, after setting all properties.
function othercourseswere_CreateFcn(hObject, eventdata, handles)
% hObject    handle to othercourseswere (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in musicperceptioncourse.
function musicperceptioncourse_Callback(hObject, eventdata, handles)
% hObject    handle to musicperceptioncourse (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of musicperceptioncourse


% --- Executes on button press in auditorycourse.
function auditorycourse_Callback(hObject, eventdata, handles)
% hObject    handle to auditorycourse (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of auditorycourse



function instrument1_Callback(hObject, eventdata, handles)
% hObject    handle to instrument1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of instrument1 as text
%        str2double(get(hObject,'String')) returns contents of instrument1 as a double


% --- Executes during object creation, after setting all properties.
function instrument1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to instrument1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function startage1_Callback(hObject, eventdata, handles)
% hObject    handle to startage1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of startage1 as text
%        str2double(get(hObject,'String')) returns contents of startage1 as a double


% --- Executes during object creation, after setting all properties.
function startage1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to startage1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function endage1_Callback(hObject, eventdata, handles)
% hObject    handle to endage1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of endage1 as text
%        str2double(get(hObject,'String')) returns contents of endage1 as a double


% --- Executes during object creation, after setting all properties.
function endage1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to endage1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in privatelessons1.
function privatelessons1_Callback(hObject, eventdata, handles)
% hObject    handle to privatelessons1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of privatelessons1


% --- Executes on button press in grouplessons1.
function grouplessons1_Callback(hObject, eventdata, handles)
% hObject    handle to grouplessons1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of grouplessons1


% --- Executes on button press in playgroup1.
function playgroup1_Callback(hObject, eventdata, handles)
% hObject    handle to playgroup1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of playgroup1


% --- Executes on button press in currentplay1.
function currentplay1_Callback(hObject, eventdata, handles)
% hObject    handle to currentplay1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of currentplay1



function instrument2_Callback(hObject, eventdata, handles)
% hObject    handle to instrument2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of instrument2 as text
%        str2double(get(hObject,'String')) returns contents of instrument2 as a double


% --- Executes during object creation, after setting all properties.
function instrument2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to instrument2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function startage2_Callback(hObject, eventdata, handles)
% hObject    handle to startage2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of startage2 as text
%        str2double(get(hObject,'String')) returns contents of startage2 as a double


% --- Executes during object creation, after setting all properties.
function startage2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to startage2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function endage2_Callback(hObject, eventdata, handles)
% hObject    handle to endage2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of endage2 as text
%        str2double(get(hObject,'String')) returns contents of endage2 as a double


% --- Executes during object creation, after setting all properties.
function endage2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to endage2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in privatelessons2.
function privatelessons2_Callback(hObject, eventdata, handles)
% hObject    handle to privatelessons2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of privatelessons2


% --- Executes on button press in grouplessons2.
function grouplessons2_Callback(hObject, eventdata, handles)
% hObject    handle to grouplessons2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of grouplessons2


% --- Executes on button press in playgroup2.
function playgroup2_Callback(hObject, eventdata, handles)
% hObject    handle to playgroup2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of playgroup2


% --- Executes on button press in currentplay2.
function currentplay2_Callback(hObject, eventdata, handles)
% hObject    handle to currentplay2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of currentplay2



function instrument3_Callback(hObject, eventdata, handles)
% hObject    handle to instrument3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of instrument3 as text
%        str2double(get(hObject,'String')) returns contents of instrument3 as a double


% --- Executes during object creation, after setting all properties.
function instrument3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to instrument3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function startage3_Callback(hObject, eventdata, handles)
% hObject    handle to startage3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of startage3 as text
%        str2double(get(hObject,'String')) returns contents of startage3 as a double


% --- Executes during object creation, after setting all properties.
function startage3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to startage3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function endage3_Callback(hObject, eventdata, handles)
% hObject    handle to endage3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of endage3 as text
%        str2double(get(hObject,'String')) returns contents of endage3 as a double


% --- Executes during object creation, after setting all properties.
function endage3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to endage3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in privatelessons3.
function privatelessons3_Callback(hObject, eventdata, handles)
% hObject    handle to privatelessons3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of privatelessons3


% --- Executes on button press in grouplessons3.
function grouplessons3_Callback(hObject, eventdata, handles)
% hObject    handle to grouplessons3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of grouplessons3


% --- Executes on button press in playgroup3.
function playgroup3_Callback(hObject, eventdata, handles)
% hObject    handle to playgroup3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of playgroup3


% --- Executes on button press in currentplay3.
function currentplay3_Callback(hObject, eventdata, handles)
% hObject    handle to currentplay3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of currentplay3



function otherinstruments_Callback(hObject, eventdata, handles)
% hObject    handle to otherinstruments (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of otherinstruments as text
%        str2double(get(hObject,'String')) returns contents of otherinstruments as a double


% --- Executes during object creation, after setting all properties.
function otherinstruments_CreateFcn(hObject, eventdata, handles)
% hObject    handle to otherinstruments (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function listenperweek_Callback(hObject, eventdata, handles)
% hObject    handle to listenperweek (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of listenperweek as text
%        str2double(get(hObject,'String')) returns contents of listenperweek as a double


% --- Executes during object creation, after setting all properties.
function listenperweek_CreateFcn(hObject, eventdata, handles)
% hObject    handle to listenperweek (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function playperweek_Callback(hObject, eventdata, handles)
% hObject    handle to playperweek (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of playperweek as text
%        str2double(get(hObject,'String')) returns contents of playperweek as a double


% --- Executes during object creation, after setting all properties.
function playperweek_CreateFcn(hObject, eventdata, handles)
% hObject    handle to playperweek (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function liketocompose_Callback(hObject, eventdata, handles)
% hObject    handle to liketocompose (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of liketocompose as text
%        str2double(get(hObject,'String')) returns contents of liketocompose as a double


% --- Executes during object creation, after setting all properties.
function liketocompose_CreateFcn(hObject, eventdata, handles)
% hObject    handle to liketocompose (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function attendmoreto_Callback(hObject, eventdata, handles)
% hObject    handle to attendmoreto (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of attendmoreto as text
%        str2double(get(hObject,'String')) returns contents of attendmoreto as a double


% --- Executes during object creation, after setting all properties.
function attendmoreto_CreateFcn(hObject, eventdata, handles)
% hObject    handle to attendmoreto (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function anythingelse_Callback(hObject, eventdata, handles)
% hObject    handle to anythingelse (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of anythingelse as text
%        str2double(get(hObject,'String')) returns contents of anythingelse as a double


% --- Executes during object creation, after setting all properties.
function anythingelse_CreateFcn(hObject, eventdata, handles)
% hObject    handle to anythingelse (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in musicmajor.
function musicmajor_Callback(hObject, eventdata, handles)
% hObject    handle to musicmajor (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of musicmajor


% --- Executes on button press in musicminor.
function musicminor_Callback(hObject, eventdata, handles)
% hObject    handle to musicminor (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of musicminor


