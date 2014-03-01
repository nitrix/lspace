#ifndef WINDOW_HPP
#define WINDOW_HPP

#include "general.hpp"
#include "SFML/Graphics/RenderWindow.hpp"

class Window : public sf::RenderWindow
{
    public:
        Window(void);
        ~Window(void);
};

#endif /* WINDOW_HPP */
