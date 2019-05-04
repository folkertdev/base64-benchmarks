module New.Decode exposing (decoder, fromBytes)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode


fromBytes : Bytes -> Maybe String
fromBytes bytes =
    Decode.decode (decoder (Bytes.width bytes)) bytes


decoder : Int -> Decode.Decoder String
decoder width =
    Decode.loop ( width, "" ) loopHelp



-- INTERNALS


loopHelp : ( Int, String ) -> Decode.Decoder (Decode.Step ( Int, String ) String)
loopHelp ( remaining, string ) =
    {- Performance Notes

       the elm/bytes package uses a DataView under the hood. These only allow reading/writing uint8, so there is no gain in
       decoding a uint16 here
    -}
    if remaining >= 3 then
        Decode.map3
            (\a b c ->
                case bitsToChars (Bitwise.shiftLeftBy 16 a + Bitwise.shiftLeftBy 8 b + c) 0 of
                    Nothing ->
                        Decode.fail

                    Just s ->
                        Decode.succeed (Decode.Loop ( remaining - 3, string ++ s ))
            )
            Decode.unsignedInt8
            Decode.unsignedInt8
            Decode.unsignedInt8
            |> Decode.andThen identity

    else if remaining == 0 then
        Decode.succeed (Decode.Done string)

    else if remaining == 2 then
        Decode.map2
            (\a b ->
                case bitsToChars (Bitwise.shiftLeftBy 16 a + Bitwise.shiftLeftBy 8 b) 1 of
                    Nothing ->
                        Decode.fail

                    Just s ->
                        Decode.succeed (Decode.Done (string ++ s))
            )
            Decode.unsignedInt8
            Decode.unsignedInt8
            |> Decode.andThen identity

    else
        -- remaining == 1
        Decode.map
            (\a ->
                case bitsToChars (Bitwise.shiftLeftBy 16 a) 2 of
                    Nothing ->
                        Decode.fail

                    Just s ->
                        Decode.succeed (Decode.Done (string ++ s))
            )
            Decode.unsignedInt8
            |> Decode.andThen identity


{-| Mask that can be used to get the lowest 6 bits of a binary number
-}
lowest6BitsMask : Int
lowest6BitsMask =
    63


{-| Turn the decoded bits (at most 24, can be fewer because of padding) into 4 base64 characters.

(- - - - - - - -)(- - - - - - - -)(- - - - - - - -)
(- - - - - -|- - - - - -|- - - - - -|- - - - - -)

-}
bitsToChars : Int -> Int -> Maybe String
bitsToChars bits missing =
    {- Performance Notes

       `String.cons` proved to be the fastest way of combining characters into a string

       The input is 24 bits, which we have to partition into 4 6-bit segments. We achieve this by
       shifting to the right by (a multiple of) 6 to remove unwanted bits on the right, then `Bitwise.and`
       with `0b111111` (which is 2^6 - 1 or 63) (so, 6 1s) to remove unwanted bits on the left.

    -}
    let
        x =
            Bitwise.shiftRightBy 18 bits

        y =
            Bitwise.and (Bitwise.shiftRightBy 12 bits) lowest6BitsMask

        z =
            Bitwise.and (Bitwise.shiftRightBy 6 bits) lowest6BitsMask

        w =
            Bitwise.and bits lowest6BitsMask

        p =
            unsafeToChar x

        q =
            unsafeToChar y

        r =
            unsafeToChar z

        s =
            unsafeToChar w
    in
    if isValidInt x && isValidInt y then
        if missing == 2 then
            Just (String.cons p (String.cons q "=="))

        else if isValidInt z then
            if missing == 1 then
                Just (String.cons p (String.cons q (String.cons r "=")))

            else if isValidInt w then
                Just (String.cons p (String.cons q (String.cons r (String.fromChar s))))

            else
                Nothing

        else
            Nothing

    else
        Nothing


isValidInt : Int -> Bool
isValidInt n =
    n >= 0 && n <= 63


unsafeToChar : Int -> Char
unsafeToChar n =
    if n <= 25 then
        Char.fromCode (65 + n)

    else if n <= 51 then
        Char.fromCode (97 + (n - 26))

    else if n <= 61 then
        Char.fromCode (48 + (n - 52))

    else
        case n of
            62 ->
                '+'

            63 ->
                '/'

            _ ->
                '\u{0000}'
