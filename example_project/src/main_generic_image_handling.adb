------------------------------------------------------------------------------------------------------------------------
--  Copyright 2024 Cre8or                                                                                             --
--                                                                                                                    --
--  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance    --
--  with the License. You may obtain a copy of the License at                                                         --
--                                                                                                                    --
--     http://www.apache.org/licenses/LICENSE-2.0                                                                     --
--                                                                                                                    --
--  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed  --
--  on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.                     --
--  See the License for the specific language governing permissions and limitations under the License.                --
------------------------------------------------------------------------------------------------------------------------





with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

with Cre8or_Generic_Image_Handling;



pragma Elaborate_All (Ada.Numerics.Discrete_Random);
pragma Elaborate_All (Ada.Text_IO);

pragma Elaborate_All (Cre8or_Generic_Image_Handling);





procedure Main_Generic_Image_Handling is



	-- Use clauses (1 / 2)
	use Ada;



	-- Types
	type T_Byte is mod 2**8;
	subtype T_Float is Float;
	subtype T_Natural is Natural;

	type T_Index_GreyA is (G, A);
	--type T_Index_RGBA is (R, G, B, A);
	type T_Index_RGBA is range -9003 .. -9000;



	-- Packages
	package P_Img is new Cre8or_Generic_Image_Handling (
		T_Byte        => T_Byte,
		T_Float       => T_Float,
		T_Size        => T_Natural,
		T_Index_GreyA => T_Index_GreyA,
		T_Index_RGBA  => T_Index_RGBA
	);

	package P_Random is new Ada.Numerics.Discrete_Random (T_Byte);



	-- Use clauses (2 / 2)
	use P_Random;

	use type P_Img.T_Image;



	-- Variables
	Handle   : P_Img.T_Image;
	Handle_2 : P_Img.T_Image;
	Handle_3 : P_Img.T_Image;

	Colour        : P_Img.T_Colour_RGB;
	Width, Height : T_Natural;
	P             : T_Float;
	Gen           : P_Random.Generator;



begin



	Text_IO.Put_Line ("Loading first image...");
	Handle.Load ("res/hello_world.png");

	-- Make a shallow copy
	Handle_3 := Handle;
	Text_IO.Put_Line ("Performed shallow copy - image objects identical: " & Boolean'Image (Handle = Handle_3));

	-- Enforce RGB format
	Text_IO.Put_Line ("Converting to RGB...");
	Handle.Convert_To (P_Img.E_RGB);

	-- Describe the image
	Width  := Handle.Get_Width;
	Height := Handle.Get_Height;
	Text_IO.Put_Line ("Colour format: " & Handle.Get_Colour_Format'Img);
	Text_IO.Put_Line ("Resolution:" & Width'Img & " x" & Height'Img);

	-- Make a deep copy
	Handle_3.Clear;
	Handle_2.Clear;
	Handle.Copy_To (Handle_2);
	Text_IO.Put_Line ("Performed deep copy - image objects identical: " & Boolean'Image (Handle = Handle_2));

	-- Generate a noisy image using RNG
	Reset (Gen);

	for Y in 1 .. Height loop
		for X in 1 .. Width loop

			P := T_Float (X) / T_Float (Width);
			Colour := (Random (Gen, 0, 100), P_Img.To_Byte (P), 0);
			Handle.Get_Data_RGB (Y, X) := Colour;

			Colour := (others => Random (Gen));
			Handle_2.Get_Data_RGB (Y, X) := Colour;

		end loop;
	end loop;

	Handle.Save ("output/output", P_Img.E_JPEG);
	Handle_2.Save ("output/output", P_Img.E_PNG);



	Text_IO.New_Line;

	-- Test colour format conversions
	for Format in P_Img.T_File_Format'Range loop

		Text_IO.Put ("Testing format " & Format'Img & "... ");

		-- Load the source image (format RGBA)
		Handle.Load ("res/hello_world.png");

		-- RGB
		Handle.Flip_Horizontally;
		Handle.Save ("output/" & P_Img.To_String (Format) & "/mlem_rgb", Format);

		-- RGBA (fill alpha with 100)
		Handle.Convert_To (P_Img.E_RGBA, 100);
		Handle.Rotate_CW;
		Handle.Save ("output/" & P_Img.To_String (Format) & "/mlem_rgba", Format);

		-- Greyscale-alpha
		Handle.Convert_To (P_Img.E_GreyA);
		Handle.Flip_Vertically;
		Handle.Save ("output/" & P_Img.To_String (Format) & "/mlem_greya", Format);

		-- Greyscale
		Handle.Convert_To (P_Img.E_Grey);
		Handle.Rotate_180;
		Handle.Save ("output/" & P_Img.To_String (Format) & "/mlem_grey", Format);

		-- Back to RGBA (fill missing channels with 255)
		Handle.Convert_To (P_Img.E_RGBA);
		Handle.Rotate_CCW;
		Handle.Save ("output/" & P_Img.To_String (Format) & "/mlem_rgba_red.", Format);

		Text_IO.Put_Line ("done!");

	end loop;



	-- Perform some simple image manipulation
	Text_IO.New_Line;
	Text_IO.Put ("Inserting images into one another... ");

	Handle.Load ("res/hello_world.png");
	Handle_2.Load ("res/mlem.jpg");

	Handle.Convert_To (P_Img.E_RGBA);
	Handle_2.Convert_To (P_Img.E_RGBA);

	-- Paste a region of Handle into Handle_2 at 10,10 ("Hello")
	Handle.Insert_Into (Handle_2, 400, 60, (
		Top    => 4,
		Left   => 8,
		Bottom => 30,
		Right  => 88

	));

	-- And another region ("world!")
	Handle.Insert_Into (Handle_2, 400, 100, (
		Top    => 18,
		Left   => 94,
		Bottom => 44,
		Right  => 193
	));

	Handle_2.Save ("output/mlem_inserted", P_Img.E_JPEG);

	Text_IO.Put_Line ("done!");
	Text_IO.Put ("Cropping image... ");

	-- Cropping Handle_2 and storing the result in Handle_3
	Handle_2.Crop (Handle_3, (
		Top    => 166,
		Left   => 182,
		Bottom => 248,
		Right  => 254
	));

	-- Additional manipulation, for the sake of it
	Handle_3.Flip_Horizontally;
	Handle_3.Rotate_CW;
	Handle_3.Save ("output/mlem_snoot", P_Img.E_PNG);

	Text_IO.Put_Line ("done!");
	Text_IO.New_Line;



end Main_Generic_Image_Handling;
