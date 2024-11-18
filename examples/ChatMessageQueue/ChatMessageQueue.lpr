program ChatMessageQueue;

{$mode objfpc}{$H+}{$J-}

{
  This example demonstrates how to use the TThreadSafeList to implement a chat message queue.

  - Three chat rooms (Threads) are running simultaneously.
  - Each room randomly adds messages to the shared list.
  - The main program shows the most recent messages every second.
  - The list is thread-safe, so multiple rooms can add messages at the same time.
}

uses
  Classes, SysUtils, DateUtils, ThreadSafeCollections.List, Math;

type
  TChatMessage = record
    Timestamp: TDateTime;  // When the message was sent
    Username: string;      // Who sent it
    Message: string;       // The actual message
    Room: string;          // Which chat room it's in
  end;

  // Simulates a chat room processor
  TChatRoom = class(TThread)  // Each chat room is a separate thread
  private
    FMessages: specialize TThreadSafeList<TChatMessage>;
    FRoomName: string;
  public
    constructor Create(AMessages: specialize TThreadSafeList<TChatMessage>;
                      ARoomName: string); reintroduce;
    procedure Execute; override;
  end;

// Implement the constructor
constructor TChatRoom.Create(AMessages: specialize TThreadSafeList<TChatMessage>;
                             ARoomName: string);
begin
  inherited Create(False);  // Create non-suspended
  FMessages := AMessages;
  FRoomName := ARoomName;
  FreeOnTerminate := True;
end;

// Implement Execute method
procedure TChatRoom.Execute;
var
  Msg: TChatMessage;
begin
  while not Terminated do
  begin
    // Simulate receiving a message
    Msg.Timestamp := Now;
    Msg.Username := 'User' + IntToStr(Random(100));
    Msg.Message := Format('Message from %s', [FRoomName]);
    Msg.Room := FRoomName;

    FMessages.Add(Msg);

    Sleep(Random(3000));  // Random delay between messages
  end;
end;

function ChatMessageComparer(const A, B: TChatMessage): Integer;
begin
  // Sort by timestamp, then by room
  Result := CompareDateTime(A.Timestamp, B.Timestamp);
  if Result = 0 then
    Result := CompareStr(A.Room, B.Room);
end;

// Simulate message processing
procedure ProcessMessages(Messages: specialize TThreadSafeList<TChatMessage>);
var
  I: Integer;
  CurrentTime: TDateTime;
begin
  CurrentTime := Now;

  // Remove messages older than 1 hour
  for I := Messages.Count - 1 downto 0 do
    if MinutesBetween(CurrentTime, Messages[I].Timestamp) > 60 then
      Messages.Delete(I);

  // Sort by timestamp, then by room
  Messages.Sort;

  // Display recent messages
  WriteLn('Recent Messages:');
  for I := Max(0, Messages.Count - 5) to Messages.Count - 1 do
    with Messages[I] do
      WriteLn(Format('[%s] %s@%s: %s',
        [FormatDateTime('hh:nn:ss', Timestamp),
         Room, Username, Message]));
end;

var
  MessageQueue: specialize TThreadSafeList<TChatMessage>;
  Rooms: array[1..3] of TChatRoom;
  Msg: TChatMessage;
  I: Integer;

begin
  Randomize;  // Initialize random number generator
  MessageQueue := specialize TThreadSafeList<TChatMessage>.Create(@ChatMessageComparer);
  try
    // Create chat rooms
    Rooms[1] := TChatRoom.Create(MessageQueue, 'General');
    Rooms[2] := TChatRoom.Create(MessageQueue, 'Support');
    Rooms[3] := TChatRoom.Create(MessageQueue, 'Development');

    // Simulate some messages
    Msg.Timestamp := Now;
    Msg.Username := 'System';
    Msg.Message := 'Chat server started';
    for I := 1 to 3 do
    begin
      Msg.Room := 'Room' + IntToStr(I);
      MessageQueue.Add(Msg);
    end;

    WriteLn('Chat server running. Press Ctrl+C to exit.');
    WriteLn;

    // Process messages every second
    while True do
    begin
      // Show last 5 messages
      ProcessMessages(MessageQueue);
      // Wait 1 second before showing the next set of messages
      Sleep(1000);
      WriteLn;  // Add blank line between updates
    end;
  finally
    MessageQueue.Free;
  end;

  WriteLn('Press enter to quit ...');
  ReadLn;
end.
