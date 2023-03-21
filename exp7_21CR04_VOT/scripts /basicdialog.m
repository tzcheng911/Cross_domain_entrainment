function varargout = basicdialog(varargin)
% BASICDIALOG M-file for basicdialog.fig
%      BASICDIALOG by itself, creates a new BASICDIALOG or raises the
%      existing singleton*.
%
%      H = BASICDIALOG returns the handle to a new BASICDIALOG or the handle to
%      the existing singleton*.
%
%      BASICDIALOG('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in BASICDIALOG.M with the given input arguments.
%
%      BASICDIALOG('Property','Value',...) creates a new BASICDIALOG or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before basicdialog_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to basicdialog_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help basicdialog

% Last Modified by GUIDE v2.5 22-Feb-2012 11:15:12

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @basicdialog_OpeningFcn, ...
                   'gui_OutputFcn',  @basicdialog_OutputFcn, ...
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

% --- Executes just before basicdialog is made visible.
function basicdialog_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to basicdialog (see VARARGIN)

% Choose default command line output for basicdialog
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

% UIWAIT makes basicdialog wait for user response (see UIRESUME)
uiwait(handles.figure1);

% --- Outputs from this function are returned to the command line.
function varargout = basicdialog_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;
gendertemp = get(handles.gender,'String');
ethtemp = get(handles.ethnicity,'String');


varargout{1} = {['Recorded on: ' datestr(clock())];...
    ['Gender: ' gendertemp{get(handles.gender,'Value')}];...
['Ethnicity: ' ethtemp{get(handles.ethnicity,'Value')}];...
['Alaska Native: ' int2str(get(handles.alaskanative,'Value'))];...
['American Indian: ' int2str(get(handles.americanindian,'Value'))];...
['Black or Afr Am: ' int2str(get(handles.blackorafricanamerican,'Value'))];...
['East Asian: ' int2str(get(handles.eastasian,'Value'))];...
['Native Hawaiian: ' int2str(get(handles.nativehawaiian,'Value'))];...
['Pacific Islander: ' int2str(get(handles.pacificislander,'Value'))];...
['South Asian: ' int2str(get(handles.southasian,'Value'))];...
['White: ' int2str(get(handles.white,'Value'))];...
['Prefer not to say: ' int2str(get(handles.prefernottosay,'Value'))];...
['Age: ' get(handles.age,'String')];};


% % % The figure can be deleted now
delete(handles.figure1);

% --- Executes on button press in pushbutton1.
function pushbutton1_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

handles.output = get(hObject,'String');

% Update handles structure
guidata(hObject, handles);

if ( (get(handles.alaskanative,'Value') + get(handles.americanindian,'Value') + ...
        get(handles.blackorafricanamerican,'Value') + get(handles.eastasian,'Value') + ...
        get(handles.nativehawaiian,'Value') + get(handles.pacificislander,'Value') + ...
        get(handles.southasian,'Value') + get(handles.white,'Value') +...
        get(handles.prefernottosay,'Value')) == 0 )
    beep
elseif (get(handles.age,'String') == '0')
    beep
elseif (get(handles.gender,'Value') == 1) % ie, the first 'non option' is selected--the 'PLease select response below' option
    beep
elseif (get(handles.ethnicity,'Value') == 1) % ie, the first 'non option' is selected
    beep
else
    % Use UIRESUME instead of delete because the OutputFcn needs
    % to get the updated handles structure.
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

if isequal(get(hObject, 'waitstatus'), 'waiting')
    % The GUI is still in UIWAIT, us UIRESUME
    uiresume(hObject);
else
    % The GUI is no longer waiting, just close it
    delete(hObject);
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

% --- Executes on selection change in gender.
function gender_Callback(hObject, eventdata, handles)
% hObject    handle to gender (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: 
contents = get(hObject,'String'); %returns popupmenu1 contents as cell array
langProf = contents{get(hObject,'Value')}; %returns selected item from popupmenu1


% --- Executes during object creation, after setting all properties.
function gender_CreateFcn(hObject, eventdata, handles)
% hObject    handle to gender (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --- Executes on selection change in ethnicity.
function ethnicity_Callback(hObject, eventdata, handles)
% hObject    handle to ethnicity (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns ethnicity contents as cell array
%        contents{get(hObject,'Value')} returns selected item from ethnicity


% --- Executes during object creation, after setting all properties.
function ethnicity_CreateFcn(hObject, eventdata, handles)
% hObject    handle to ethnicity (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --- Executes on button press in alaskanative.
function alaskanative_Callback(hObject, eventdata, handles)
% hObject    handle to alaskanative (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of alaskanative


% --- Executes on button press in americanindian.
function americanindian_Callback(hObject, eventdata, handles)
% hObject    handle to americanindian (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of americanindian


% --- Executes on button press in blackorafricanamerican.
function blackorafricanamerican_Callback(hObject, eventdata, handles)
% hObject    handle to blackorafricanamerican (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of blackorafricanamerican


% --- Executes on button press in eastasian.
function eastasian_Callback(hObject, eventdata, handles)
% hObject    handle to eastasian (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of eastasian


% --- Executes on button press in nativehawaiian.
function nativehawaiian_Callback(hObject, eventdata, handles)
% hObject    handle to nativehawaiian (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of nativehawaiian


% --- Executes on button press in pacificislander.
function pacificislander_Callback(hObject, eventdata, handles)
% hObject    handle to pacificislander (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of pacificislander


% --- Executes on button press in southasian.
function southasian_Callback(hObject, eventdata, handles)
% hObject    handle to southasian (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of southasian


% --- Executes on button press in white.
function white_Callback(hObject, eventdata, handles)
% hObject    handle to white (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of white


% --- Executes on button press in prefernottosay.
function prefernottosay_Callback(hObject, eventdata, handles)
% hObject    handle to prefernottosay (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of prefernottosay



function age_Callback(hObject, eventdata, handles)
% hObject    handle to age (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of age as text
%        str2double(get(hObject,'String')) returns contents of age as a double


% --- Executes during object creation, after setting all properties.
function age_CreateFcn(hObject, eventdata, handles)
% hObject    handle to age (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


