function varargout = LanguageBackgroundBDS(varargin)
% LANGUAGEBACKGROUNDBDS M-file for LanguageBackgroundBDS.fig
%      LANGUAGEBACKGROUNDBDS by itself, creates a new LANGUAGEBACKGROUNDBDS or raises the
%      existing singleton*.
%
%      H = LANGUAGEBACKGROUNDBDS returns the handle to a new LANGUAGEBACKGROUNDBDS or the handle to
%      the existing singleton*.
%
%      LANGUAGEBACKGROUNDBDS('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in LANGUAGEBACKGROUNDBDS.M with the given input arguments.
%
%      LANGUAGEBACKGROUNDBDS('Property','Value',...) creates a new LANGUAGEBACKGROUNDBDS or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before LanguageBackgroundBDS_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to LanguageBackgroundBDS_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help LanguageBackgroundBDS

% Last Modified by GUIDE v2.5 06-Feb-2012 16:31:59

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @LanguageBackgroundBDS_OpeningFcn, ...
                   'gui_OutputFcn',  @LanguageBackgroundBDS_OutputFcn, ...
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

% --- Executes just before LanguageBackgroundBDS is made visible.
function LanguageBackgroundBDS_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to LanguageBackgroundBDS (see VARARGIN)

% Choose default command line output for LanguageBackgroundBDS
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

% UIWAIT makes LanguageBackgroundBDS wait for user response (see UIRESUME)
uiwait(handles.figure1);

% --- Outputs from this function are returned to the command line.
function varargout = LanguageBackgroundBDS_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
%varargout{1} = handles.output;
%temp1 = get(handles.popupmenu1,'String');
varargout{1} = {['Recorded on: ' datestr(clock())];...
    ['Born in: ' get(handles.bornin,'String')];...
    ['Live in: ' get(handles.livein,'String')];...
['Move 1: ' get(handles.move1,'String')];...
['Move 2: ' get(handles.move2,'String')];...
['Move 3: ' get(handles.move3,'String')];...
['Move 4: ' get(handles.move4,'String')];...
['Move 5: ' get(handles.move5,'String')];...
['Move 6: ' get(handles.move6,'String')];...
['Move 7: ' get(handles.move7,'String')];...

['Dominant language: ' get(handles.domlang,'String')];...
['Learned DL from: ' get(handles.domlearn,'String')];...
['First started learning DL: ' get(handles.domacqage,'String')];...
['First felt comfortable with DL: ' get(handles.domcomfage,'String')];...
['Years of DL full-time school: ' get(handles.domfulltime,'String')];...
['Years of DL weekend school: ' get(handles.domweekend,'String')];...
['Years of DL once-a-day school: ' get(handles.domonceday,'String')];...

['Next-most-dominant language: ' get(handles.nextlang,'String')];...
['Learned NL from: ' get(handles.nextlang,'String')];...
['First started learning NL: ' get(handles.nextacqage,'String')];...
['First felt comfortable with NL: ' get(handles.nextcomfage,'String')];...
['Years of NL full-time school: ' get(handles.nextfulltime,'String')];...
['Years of NL weekend school: ' get(handles.nextweekend,'String')];...
['Years of NL once-a-day school: ' get(handles.nextonceday,'String')];...

['Languages used at home: ' get(handles.homelang,'String')];...
['Languages used in dorm: ' get(handles.dormlang,'String')];...
['Language used to do math: ' get(handles.mathlang,'String')];...
['Language you think in : ' get(handles.thinklang,'String')];...
['Accented English?: ' get(handles.accinenglish,'String')];...
['English accent in other language?: ' get(handles.accinother,'String')];...

['Pick a language for life: ' get(handles.lifelang,'String')];...
['Have lost fluency?: ' get(handles.fluencyloss,'String')];...
['Language lost fluency in: ' get(handles.losslang,'String')];...
['Age of fluency loss: ' get(handles.lossage,'String')];...
['Other fluent languages: ' get(handles.otherfluent,'String')];...

['Any other info: ' get(handles.otherinfo,'String')]};




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
% to get the updated handles structure.
uiresume(handles.figure1);

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



function bornin_Callback(hObject, eventdata, handles)
% hObject    handle to bornin (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of bornin as text
%        str2double(get(hObject,'String')) returns contents of bornin as a double


% --- Executes during object creation, after setting all properties.
function bornin_CreateFcn(hObject, eventdata, handles)
% hObject    handle to bornin (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
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






function livein_Callback(hObject, eventdata, handles)
% hObject    handle to livein (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of livein as text
%        str2double(get(hObject,'String')) returns contents of livein as a double


% --- Executes during object creation, after setting all properties.
function livein_CreateFcn(hObject, eventdata, handles)
% hObject    handle to livein (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end





function move1_Callback(hObject, eventdata, handles)
% hObject    handle to move1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of move1 as text
%        str2double(get(hObject,'String')) returns contents of move1 as a double


% --- Executes during object creation, after setting all properties.
function move1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to move1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function move2_Callback(hObject, eventdata, handles)
% hObject    handle to move2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of move2 as text
%        str2double(get(hObject,'String')) returns contents of move2 as a double


% --- Executes during object creation, after setting all properties.
function move2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to move2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function move3_Callback(hObject, eventdata, handles)
% hObject    handle to move3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of move3 as text
%        str2double(get(hObject,'String')) returns contents of move3 as a double


% --- Executes during object creation, after setting all properties.
function move3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to move3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function move4_Callback(hObject, eventdata, handles)
% hObject    handle to move4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of move4 as text
%        str2double(get(hObject,'String')) returns contents of move4 as a double


% --- Executes during object creation, after setting all properties.
function move4_CreateFcn(hObject, eventdata, handles)
% hObject    handle to move4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function move5_Callback(hObject, eventdata, handles)
% hObject    handle to move5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of move5 as text
%        str2double(get(hObject,'String')) returns contents of move5 as a double


% --- Executes during object creation, after setting all properties.
function move5_CreateFcn(hObject, eventdata, handles)
% hObject    handle to move5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function move6_Callback(hObject, eventdata, handles)
% hObject    handle to move6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of move6 as text
%        str2double(get(hObject,'String')) returns contents of move6 as a double


% --- Executes during object creation, after setting all properties.
function move6_CreateFcn(hObject, eventdata, handles)
% hObject    handle to move6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function move7_Callback(hObject, eventdata, handles)
% hObject    handle to move7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of move7 as text
%        str2double(get(hObject,'String')) returns contents of move7 as a double


% --- Executes during object creation, after setting all properties.
function move7_CreateFcn(hObject, eventdata, handles)
% hObject    handle to move7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function domlang_Callback(hObject, eventdata, handles)
% hObject    handle to domlang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of domlang as text
%        str2double(get(hObject,'String')) returns contents of domlang as a double


% --- Executes during object creation, after setting all properties.
function domlang_CreateFcn(hObject, eventdata, handles)
% hObject    handle to domlang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function domlearn_Callback(hObject, eventdata, handles)
% hObject    handle to domlearn (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of domlearn as text
%        str2double(get(hObject,'String')) returns contents of domlearn as a double


% --- Executes during object creation, after setting all properties.
function domlearn_CreateFcn(hObject, eventdata, handles)
% hObject    handle to domlearn (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function domacqage_Callback(hObject, eventdata, handles)
% hObject    handle to domacqage (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of domacqage as text
%        str2double(get(hObject,'String')) returns contents of domacqage as a double


% --- Executes during object creation, after setting all properties.
function domacqage_CreateFcn(hObject, eventdata, handles)
% hObject    handle to domacqage (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function domcomfage_Callback(hObject, eventdata, handles)
% hObject    handle to domcomfage (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of domcomfage as text
%        str2double(get(hObject,'String')) returns contents of domcomfage as a double


% --- Executes during object creation, after setting all properties.
function domcomfage_CreateFcn(hObject, eventdata, handles)
% hObject    handle to domcomfage (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function domfulltime_Callback(hObject, eventdata, handles)
% hObject    handle to domfulltime (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of domfulltime as text
%        str2double(get(hObject,'String')) returns contents of domfulltime as a double


% --- Executes during object creation, after setting all properties.
function domfulltime_CreateFcn(hObject, eventdata, handles)
% hObject    handle to domfulltime (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function domweekend_Callback(hObject, eventdata, handles)
% hObject    handle to domweekend (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of domweekend as text
%        str2double(get(hObject,'String')) returns contents of domweekend as a double


% --- Executes during object creation, after setting all properties.
function domweekend_CreateFcn(hObject, eventdata, handles)
% hObject    handle to domweekend (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function domonceday_Callback(hObject, eventdata, handles)
% hObject    handle to domonceday (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of domonceday as text
%        str2double(get(hObject,'String')) returns contents of domonceday as a double


% --- Executes during object creation, after setting all properties.
function domonceday_CreateFcn(hObject, eventdata, handles)
% hObject    handle to domonceday (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end




function nextlang_Callback(hObject, eventdata, handles)
% hObject    handle to nextlang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of nextlang as text
%        str2double(get(hObject,'String')) returns contents of nextlang as a double


% --- Executes during object creation, after setting all properties.
function nextlang_CreateFcn(hObject, eventdata, handles)
% hObject    handle to nextlang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function nextlearn_Callback(hObject, eventdata, handles)
% hObject    handle to nextlearn (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of nextlearn as text
%        str2double(get(hObject,'String')) returns contents of nextlearn as a double


% --- Executes during object creation, after setting all properties.
function nextlearn_CreateFcn(hObject, eventdata, handles)
% hObject    handle to nextlearn (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function nextacqage_Callback(hObject, eventdata, handles)
% hObject    handle to nextacqage (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of nextacqage as text
%        str2double(get(hObject,'String')) returns contents of nextacqage as a double


% --- Executes during object creation, after setting all properties.
function nextacqage_CreateFcn(hObject, eventdata, handles)
% hObject    handle to nextacqage (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function nextcomfage_Callback(hObject, eventdata, handles)
% hObject    handle to nextcomfage (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of nextcomfage as text
%        str2double(get(hObject,'String')) returns contents of nextcomfage as a double


% --- Executes during object creation, after setting all properties.
function nextcomfage_CreateFcn(hObject, eventdata, handles)
% hObject    handle to nextcomfage (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function nextfulltime_Callback(hObject, eventdata, handles)
% hObject    handle to nextfulltime (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of nextfulltime as text
%        str2double(get(hObject,'String')) returns contents of nextfulltime as a double


% --- Executes during object creation, after setting all properties.
function nextfulltime_CreateFcn(hObject, eventdata, handles)
% hObject    handle to nextfulltime (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function nextweekend_Callback(hObject, eventdata, handles)
% hObject    handle to nextweekend (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of nextweekend as text
%        str2double(get(hObject,'String')) returns contents of nextweekend as a double


% --- Executes during object creation, after setting all properties.
function nextweekend_CreateFcn(hObject, eventdata, handles)
% hObject    handle to nextweekend (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function nextonceday_Callback(hObject, eventdata, handles)
% hObject    handle to nextonceday (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of nextonceday as text
%        str2double(get(hObject,'String')) returns contents of nextonceday as a double


% --- Executes during object creation, after setting all properties.
function nextonceday_CreateFcn(hObject, eventdata, handles)
% hObject    handle to nextonceday (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function homelang_Callback(hObject, eventdata, handles)
% hObject    handle to homelang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of homelang as text
%        str2double(get(hObject,'String')) returns contents of homelang as a double


% --- Executes during object creation, after setting all properties.
function homelang_CreateFcn(hObject, eventdata, handles)
% hObject    handle to homelang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function dormlang_Callback(hObject, eventdata, handles)
% hObject    handle to dormlang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of dormlang as text
%        str2double(get(hObject,'String')) returns contents of dormlang as a double


% --- Executes during object creation, after setting all properties.
function dormlang_CreateFcn(hObject, eventdata, handles)
% hObject    handle to dormlang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function mathlang_Callback(hObject, eventdata, handles)
% hObject    handle to mathlang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of mathlang as text
%        str2double(get(hObject,'String')) returns contents of mathlang as a double


% --- Executes during object creation, after setting all properties.
function mathlang_CreateFcn(hObject, eventdata, handles)
% hObject    handle to mathlang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function thinklang_Callback(hObject, eventdata, handles)
% hObject    handle to thinklang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of thinklang as text
%        str2double(get(hObject,'String')) returns contents of thinklang as a double


% --- Executes during object creation, after setting all properties.
function thinklang_CreateFcn(hObject, eventdata, handles)
% hObject    handle to thinklang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function accinenglish_Callback(hObject, eventdata, handles)
% hObject    handle to accinenglish (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of accinenglish as text
%        str2double(get(hObject,'String')) returns contents of accinenglish as a double


% --- Executes during object creation, after setting all properties.
function accinenglish_CreateFcn(hObject, eventdata, handles)
% hObject    handle to accinenglish (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function accinother_Callback(hObject, eventdata, handles)
% hObject    handle to accinother (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of accinother as text
%        str2double(get(hObject,'String')) returns contents of accinother as a double


% --- Executes during object creation, after setting all properties.
function accinother_CreateFcn(hObject, eventdata, handles)
% hObject    handle to accinother (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function lifelang_Callback(hObject, eventdata, handles)
% hObject    handle to fluencyloss (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of fluencyloss as text
%        str2double(get(hObject,'String')) returns contents of fluencyloss as a double


% --- Executes during object creation, after setting all properties.
function lifelang_CreateFcn(hObject, eventdata, handles)
% hObject    handle to fluencyloss (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function fluencyloss_Callback(hObject, eventdata, handles)
% hObject    handle to fluencyloss (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of fluencyloss as text
%        str2double(get(hObject,'String')) returns contents of fluencyloss as a double


% --- Executes during object creation, after setting all properties.
function fluencyloss_CreateFcn(hObject, eventdata, handles)
% hObject    handle to fluencyloss (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function losslang_Callback(hObject, eventdata, handles)
% hObject    handle to losslang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of losslang as text
%        str2double(get(hObject,'String')) returns contents of losslang as a double


% --- Executes during object creation, after setting all properties.
function losslang_CreateFcn(hObject, eventdata, handles)
% hObject    handle to losslang (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function lossage_Callback(hObject, eventdata, handles)
% hObject    handle to lossage (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of lossage as text
%        str2double(get(hObject,'String')) returns contents of lossage as a double


% --- Executes during object creation, after setting all properties.
function lossage_CreateFcn(hObject, eventdata, handles)
% hObject    handle to lossage (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function otherfluent_Callback(hObject, eventdata, handles)
% hObject    handle to otherfluent (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of otherfluent as text
%        str2double(get(hObject,'String')) returns contents of otherfluent as a double


% --- Executes during object creation, after setting all properties.
function otherfluent_CreateFcn(hObject, eventdata, handles)
% hObject    handle to otherfluent (see GCBO)
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


