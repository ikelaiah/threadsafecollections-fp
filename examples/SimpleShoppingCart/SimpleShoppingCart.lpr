program SimpleShoppingCart;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, ThreadSafeCollections.List;

type
  TProduct = record
    Name: string;
    Price: Currency;
  end;

function ProductComparer(const A, B: TProduct): Integer;
begin
  Result := CompareStr(A.Name, B.Name);
end;

var
  Cart: specialize TThreadSafeList<TProduct>;
  Product: TProduct;
  Total: Currency;
  I: Integer;
begin
  Cart := specialize TThreadSafeList<TProduct>.Create(@ProductComparer);
  try
    // Add some products
    Product.Name := 'Apple'; Product.Price := 0.50;
    Cart.Add(Product);
    Product.Name := 'Banana'; Product.Price := 0.30;
    Cart.Add(Product);
    Product.Name := 'Orange'; Product.Price := 0.40;
    Cart.Add(Product);

    // Sort products by name
    Cart.Sort;

    // Display cart contents
    WriteLn('Shopping Cart:');
    WriteLn('-------------');
    Total := 0;
    for I := 0 to Cart.Count - 1 do
    begin
      WriteLn(Format('%s: $%.2f', [Cart[I].Name, Cart[I].Price]));
      Total := Total + Cart[I].Price;
    end;
    WriteLn('-------------');
    WriteLn(Format('Total: $%.2f', [Total]));
  finally
    Cart.Free;
  end;

  WriteLn('Press enter kay to quit ...');
  ReadLn;
end.
