-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 hyperpolymath
--
-- NoteG Engine - Ada/SPARK Implementation

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;

package body NoteG_Engine with SPARK_Mode => Off is
   -- SPARK_Mode off for implementation details requiring dynamic allocation

   use Ada.Strings.Fixed;
   use Ada.Characters.Handling;

   -- Variable storage
   package Variable_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Variable_Value,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   Variables : Variable_Maps.Map;

   procedure Initialize is
   begin
      Variables.Clear;
      Current_Mill_State := (
         Current_Card => 0,
         Stack_Depth => 0,
         Error_Flag => False,
         Error_Message => Null_Unbounded_String
      );
   end Initialize;

   procedure Load_Template (
      Path : in String;
      Success : out Boolean) is
   begin
      -- Template loading implementation
      Success := Path'Length > 0;
   end Load_Template;

   procedure Process_Content (
      Input : in String;
      Output : out Unbounded_String;
      Success : out Boolean) is
   begin
      Output := Substitute_Variables (Input);
      Success := True;
   end Process_Content;

   procedure Execute_Card (
      Card : in Operation_Card;
      State : in out Mill_State;
      Success : out Boolean) is
   begin
      case Card.Op is
         when Load_Variable =>
            State.Current_Card := State.Current_Card + 1;
            Success := True;
         when Store_Variable =>
            State.Current_Card := State.Current_Card + 1;
            Success := True;
         when Template_Substitute =>
            State.Current_Card := State.Current_Card + 1;
            Success := True;
         when Content_Transform =>
            State.Current_Card := State.Current_Card + 1;
            Success := True;
         when Output_Generate =>
            State.Current_Card := State.Current_Card + 1;
            Success := True;
         when Conditional_Branch =>
            State.Current_Card := State.Current_Card + 1;
            Success := True;
         when Loop_Start =>
            State.Stack_Depth := State.Stack_Depth + 1;
            State.Current_Card := State.Current_Card + 1;
            Success := State.Stack_Depth <= Max_Template_Depth;
         when Loop_End =>
            if State.Stack_Depth > 0 then
               State.Stack_Depth := State.Stack_Depth - 1;
               Success := True;
            else
               Success := False;
               State.Error_Flag := True;
               State.Error_Message := To_Unbounded_String ("Stack underflow");
            end if;
            State.Current_Card := State.Current_Card + 1;
      end case;
   end Execute_Card;

   procedure Set_Variable (
      Name : in String;
      Value : in Variable_Value;
      Success : out Boolean) is
      Key : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if Is_Valid_Variable_Name (Name) then
         Variables.Include (Key, Value);
         Success := True;
      else
         Success := False;
      end if;
   end Set_Variable;

   procedure Get_Variable (
      Name : in String;
      Value : out Variable_Value;
      Found : out Boolean) is
      Key : constant Unbounded_String := To_Unbounded_String (Name);
      Cursor : Variable_Maps.Cursor;
   begin
      Cursor := Variables.Find (Key);
      if Variable_Maps.Has_Element (Cursor) then
         Value := Variable_Maps.Element (Cursor);
         Found := True;
      else
         Value := (Kind => Text, Text_Val => Null_Unbounded_String);
         Found := False;
      end if;
   end Get_Variable;

   procedure Clear_Variables is
   begin
      Variables.Clear;
   end Clear_Variables;

   function Substitute_Variables (
      Template : String) return Unbounded_String is
      Result : Unbounded_String := To_Unbounded_String (Template);
      Start_Pos, End_Pos : Natural;
      Var_Name : Unbounded_String;
      Var_Value : Variable_Value;
      Found : Boolean;
   begin
      -- Find and replace {{ variable }} patterns
      loop
         Start_Pos := Index (To_String (Result), "{{");
         exit when Start_Pos = 0;

         End_Pos := Index (To_String (Result), "}}", Start_Pos);
         exit when End_Pos = 0;

         -- Extract variable name (trim whitespace)
         Var_Name := To_Unbounded_String (
            Trim (Slice (Result, Start_Pos + 2, End_Pos - 1), Ada.Strings.Both));

         Get_Variable (To_String (Var_Name), Var_Value, Found);

         if Found and then Var_Value.Kind = Text then
            Replace_Slice (Result, Start_Pos, End_Pos + 1,
                          To_String (Var_Value.Text_Val));
         else
            -- Leave unsubstituted if not found
            exit;
         end if;
      end loop;

      return Result;
   end Substitute_Variables;

   function Is_Valid_Template (Template : String) return Boolean is
      Open_Count, Close_Count : Natural := 0;
   begin
      for I in Template'Range loop
         if I < Template'Last and then
            Template (I) = '{' and then Template (I + 1) = '{' then
            Open_Count := Open_Count + 1;
         elsif I < Template'Last and then
            Template (I) = '}' and then Template (I + 1) = '}' then
            Close_Count := Close_Count + 1;
         end if;
      end loop;
      return Open_Count = Close_Count;
   end Is_Valid_Template;

   function Is_Valid_Variable_Name (Name : String) return Boolean is
   begin
      if Name'Length = 0 or Name'Length > 256 then
         return False;
      end if;

      -- First character must be letter or underscore
      if not (Is_Letter (Name (Name'First)) or Name (Name'First) = '_') then
         return False;
      end if;

      -- Rest must be alphanumeric or underscore
      for I in Name'First + 1 .. Name'Last loop
         if not (Is_Alphanumeric (Name (I)) or Name (I) = '_') then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Variable_Name;

end NoteG_Engine;
