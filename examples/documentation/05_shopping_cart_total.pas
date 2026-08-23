program ShoppingCartTotal;

{$mode objfpc}{$H+}

uses
  SysUtils,
  ThreadSafeCollections.List;

type
  TItem = record
    Name: string;
    Price: Double;
  end;

function MakeItem(const AName: string; APrice: Double): TItem;
begin
  Result.Name := AName;
  Result.Price := APrice;
end;

function CompareItem(const A, B: TItem): Integer;
begin
  Result := CompareText(A.Name, B.Name);
end;

var
  Cart: specialize TThreadSafeList<TItem>;
  Total: Double;
  Item: TItem;
begin
  Cart := specialize TThreadSafeList<TItem>.Create(@CompareItem);
  try
    Cart.Add(MakeItem('eraser', 0.5));
    Cart.Add(MakeItem('pen', 1.25));
    Cart.Add(MakeItem('paper', 2.00));

    Total := 0;
    for Item in Cart do
      Total := Total + Item.Price;
    Writeln(Total:0:2);
    Writeln(Cart.Count);
  finally
    Cart.Free;
  end;
end.
