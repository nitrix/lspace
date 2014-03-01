#include "main.hpp"

int main(int argc, const char **argv)
{
    Window window;

    sf::Event event;
    while (window.waitEvent(event))
    {
        switch (event.type)
        {
            case sf::Event::Closed:
                window.close();
                break;
            case sf::Event::KeyPressed:
                window.close();
                break;
        }
    }

    return 0;
}
