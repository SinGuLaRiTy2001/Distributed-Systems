%% Default Base File

-module(gui).
-define(width, 400).
-define(height, 400).
-export([start/2]).
-include_lib("wx/include/wx.hrl").

start(Title, Master) ->
  spawn_link(fun() -> init(Title, Master) end).

init(Title, Master) ->
  Window = make_window(Title),
  loop(Window, Master).

make_window(Title) ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, -1, Title, [{size,{?width, ?height}}]),
  wxFrame:setBackgroundColour(Frame, ?wxBLACK),
  Window = wxWindow:new(Frame, ?wxID_ANY),
  wxFrame:show(Frame),
  wxWindow:setBackgroundColour(Window, ?wxBLACK),
  wxWindow:show(Window),
  wxFrame:connect(Frame, close_window),
  Window.

loop(Window, Master)->
  receive
    #wx{event=#wxClose{}} ->
      wxWindow:destroy(Window),
      Master ! stop,
      ok;
    {color, Color} ->
      color(Window, Color),
      loop(Window, Master);
    stop ->
      ok;
    Error ->
      io:format("gui: strange message ~w ~n", [Error]),
      loop(Window, Master)
  end.

color(Window, Color) ->
  wxWindow:setBackgroundColour(Window, Color),
  wxWindow:refresh(Window).